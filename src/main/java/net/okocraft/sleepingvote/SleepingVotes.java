package net.okocraft.sleepingvote;

import com.github.siroshun09.configapi.api.Configuration;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.World;
import org.bukkit.boss.BarColor;
import org.bukkit.boss.BarStyle;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.world.TimeSkipEvent;

public class SleepingVotes {

    private static Configuration config;

    private static final Map<UUID, SleepingVotes> cache = new ConcurrentHashMap<>();

    private final Map<UUID, Boolean> voteState = new ConcurrentHashMap<>();
    private final UUID worldUid;
    private final AtomicInteger remainingTime = new AtomicInteger(-1);
    private final I18nBossBar remainingTimeBar;
    private final AtomicBoolean nightSkipping = new AtomicBoolean(false);
    private final AtomicLong previousNoSkip = new AtomicLong(-1);

    public static void onPluginEnabled(SleepingVotePlugin plugin) {
        SleepingVotes.config = plugin.getConfiguration();
    }

    public static void onPluginDisabled() {
        cache.values().forEach(v -> v.remainingTimeBar.removeAll());
    }

    public static boolean canSleep(World world) {
        long time = world.getTime();
        return world.isThundering()
                || (world.hasStorm() && 12010 <= time && time < 23992)
                || (12542 <= time && time < 23460);
    }

    private SleepingVotes(World world) {
        this.worldUid = world.getUID();
        this.remainingTimeBar = new I18nBossBar(Component.text(""), BarColor.BLUE, BarStyle.SOLID);
    }

    public static SleepingVotes getOrCreateSleepingVotes(World world) {
        if (!Bukkit.getWorlds().contains(world)) {
            throw new IllegalStateException("This world is not loaded now.");
        }
        return cache.computeIfAbsent(world.getUID(), u -> new SleepingVotes(world));
    }

    public boolean isNightSkipping() {
        return nightSkipping.get();
    }

    public boolean isVoteStarted() {
        if (remainingTime.get() < 0) {
            return false;
        }

        if (nightSkipping.get()) {
            return false;
        }

        World world = getWorld();
        if (world == null) {
            return false;
        }

        if (!canSleep(world)) {
            return false;
        }

        return true;
    }

    public boolean isVoteEndedTonight() {
        if (isVoteStarted()) {
            return false;
        }
        long previousNoSkip = this.previousNoSkip.get();
        return previousNoSkip != -1L && previousNoSkip / 24000 >= getWorld().getFullTime() / 24000;
    }

    public boolean canStartVote() {
        if (remainingTime.get() >= 0) {
            return false;
        }

        if (nightSkipping.get()) {
            return false;
        }

        World world = getWorld();
        if (world == null) {
            return false;
        }

        if (!canSleep(world)) {
            return false;
        }

        long previousNoSkip = this.previousNoSkip.get();
        if (previousNoSkip != -1L && previousNoSkip / 24000 >= getWorld().getFullTime() / 24000) {
            return false;
        }

        return true;
    }

    private static int getConfigVotingExpire() {
        return config.getInteger("voting-time", 50);
    }

    public boolean startVote() {
        if (canStartVote()) {
            remainingTime.set(getConfigVotingExpire());
            voteState.clear();
            return true;
        }
        return false;
    }

    public int getUnskippableNightInterval() {
        World world = getWorld();
        if (world == null) {
            return -1;
        }
        int interval = config.getInteger("no-skip-night-interval", 3);
        if (interval <= -1) {
            return -1;
        }
        return interval - ((int) world.getFullTime() / 24000) % (interval + 1);
    }

    public World getWorld() {
        return Bukkit.getWorld(worldUid);
    }

    public void vote(Player voter, boolean skip) {
        if (isVoteStarted() && getWorld().getPlayers().contains(voter)) {
            voteState.put(voter.getUniqueId(), skip);
        }
    }

    public void cancelVote(Player player) {
        if (isVoteStarted()) {
            voteState.remove(player.getUniqueId());
        }
    }

    public Boolean getVoteState(Player voter) {
        return isVoteStarted() ? voteState.get(voter.getUniqueId()) : null;
    }

    public Map<Boolean, Long> getVoteStates() {
        if (!isVoteStarted()) {
            return Map.of(true, 0L, false, 0L);
        }
        return getVoteStates0();
    }

    private Map<Boolean, Long> getVoteStates0() {
        return voteState.entrySet().stream()
                .collect(Collectors.partitioningBy(Map.Entry::getValue, Collectors.counting()));
    }

    public boolean tally() {
        Map<Boolean, Long> counting = getVoteStates0();
        long skipCount = counting.get(true);
        long noSkipCount = counting.get(false);
        if (skipCount == 0 && noSkipCount == 0) {
            return false;
        }
        return (double) skipCount / (skipCount + noSkipCount) > config.getDouble("skip-ratio", 0.5D);
    }

    public SkipState countTimeOne() {
        World world = getWorld();
        if (!isVoteStarted()) {
            return SkipState.NIGHT_NOT_SKIPPED;
        }

        int currentRemainingTime;
        if (voteState.size() == world.getPlayerCount()) {
            currentRemainingTime = -1;
            remainingTime.set(-1);
        } else {
            currentRemainingTime = remainingTime.addAndGet(-1);
        }

        updateTimeLeftBar(world, currentRemainingTime);

        if (currentRemainingTime < 0) {
            if (tally()) {
                skipNight();
                return SkipState.NIGHT_SKIPPED;
            } else {
                previousNoSkip.set(world.getFullTime());
                return SkipState.NIGHT_NOT_SKIPPED;
            }
        }
        return SkipState.VOTE_CONTINUES;
    }

    private synchronized void updateTimeLeftBar(World world, int remainingTime) {
        if (remainingTime < 0) {
            remainingTimeBar.removeAll();
            return;
        }

        int expire = getConfigVotingExpire();
        Set<Player> worldPlayers = new HashSet<>(world.getPlayers());
        Set<Player> barPlayers = new HashSet<>(remainingTimeBar.getPlayers());
        barPlayers.stream().filter(Predicate.not(worldPlayers::contains)).forEach(remainingTimeBar::removePlayer);
        worldPlayers.stream().filter(Predicate.not(barPlayers::contains)).forEach(remainingTimeBar::addPlayer);
        int interval = getUnskippableNightInterval();
        if (interval <= 0) {
            remainingTimeBar.setTitle(MessageKeys.getSkipBarTitle(remainingTime, expire));
        } else {
            remainingTimeBar.setTitle(MessageKeys.getSkipBarTitle(remainingTime, expire, interval));
        }
        remainingTimeBar.setProgress(remainingTime / (double) expire);
    }

    private void skipNight() {
        World world = getWorld();
        if (world == null) {
            return;
        }
        nightSkipping.set(true);

        SleepingVotePlugin plugin = SleepingVotePlugin.getPlugin(SleepingVotePlugin.class);

        world.getPlayers().stream().filter(LivingEntity::isSleeping).findAny().ifPresent(sleeper -> {
            Map<Player, Boolean> ignoreState = new ConcurrentHashMap<>();
            for (Player p : world.getPlayers()) {
                p.getScheduler().run(plugin, t -> {
                    ignoreState.put(p, p.isSleepingIgnored());
                    if (sleeper.equals(p)) {
                        p.setSleepingIgnored(false);
                    } else {
                        p.setSleepingIgnored(true);
                    }
                }, null);

                p.getScheduler().execute(
                        plugin,
                        () -> p.setSleepingIgnored(ignoreState.getOrDefault(p, false)),
                        null,
                        110L
                );
            }
        });

        Bukkit.getGlobalRegionScheduler().runDelayed(plugin, t -> {
            if (canSleep(world) && new TimeSkipEvent(
                    world,
                    TimeSkipEvent.SkipReason.NIGHT_SKIP,
                    24000 - world.getTime()
            ).callEvent()) {
                if (world.isThundering()) {
                    world.setThundering(false);
                }
                if (world.hasStorm()) {
                    world.setStorm(false);
                }
                world.setTime(0);
            }
            nightSkipping.set(false);
        }, 110L);
    }
}
