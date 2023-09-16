package net.okocraft.sleepingvote;

import com.github.siroshun09.configapi.api.Configuration;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.World;
import org.bukkit.boss.BarColor;
import org.bukkit.boss.BarStyle;
import org.bukkit.boss.BossBar;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.world.TimeSkipEvent;

public class SleepingVotes {

    private static Configuration config;

    private static final Map<UUID, SleepingVotes> currentVote = new ConcurrentHashMap<>();
    private static final Set<UUID> isNightSkipping = ConcurrentHashMap.newKeySet();
    private static final Map<UUID, Long> previousWorldNightSkip = new ConcurrentHashMap<>();
    private static final Map<UUID, Long> previousWorldNightNoSkip = new ConcurrentHashMap<>();

    private final Map<UUID, Boolean> voteState = new ConcurrentHashMap<>();
    private final UUID worldUid;
    private final AtomicInteger votedTime = new AtomicInteger(-1);
    private final I18nBossBar timeLeftBar;

    private SleepingVotes(World world) {
        this.worldUid = world.getUID();
        this.timeLeftBar = new I18nBossBar(Component.text(""), BarColor.BLUE, BarStyle.SOLID);
    }

    public static SleepingVotes getOrCreateSleepingVotes(World world) {
        return currentVote.computeIfAbsent(world.getUID(), u -> new SleepingVotes(world));
    }

    public static boolean isSleepingVoteStarted(World world) {
        return currentVote.containsKey(world.getUID());
    }

    public static boolean isVoteEnded(World world) {
        if (isNightSkipping(world)) {
            return true;
        }
        long previousNoSkip = previousWorldNightNoSkip.getOrDefault(world.getUID(), -1L);
        return previousNoSkip != -1L && previousNoSkip / 24000 >= world.getFullTime() / 24000;
    }

    public static long getPreviousNightSkip(World world) {
        return previousWorldNightSkip.getOrDefault(world.getUID(), -1L);
    }

    public static boolean canSleep(World world) {
        long time = world.getTime();
        return world.isThundering()
                || (world.hasStorm() && 12010 <= time && time < 23992)
                || (12542 <= time && time < 23460);
    }

    public static void onPluginEnabled(SleepingVotePlugin plugin) {
        SleepingVotes.config = plugin.getConfiguration();
    }

    public static void onPluginDisabled() {
        for (World world : Bukkit.getWorlds()) {
            if (isSleepingVoteStarted(world)) {
                getOrCreateSleepingVotes(world).endVote();
            }
        }
    }

    public static boolean isNightSkipping(World world) {
        return isNightSkipping.contains(world.getUID());
    }

    public World getWorld() {
        return Bukkit.getWorld(worldUid);
    }

    public NightSkipState countTimeOne(int expire) {
        World world = getWorld();
        if (world == null) {
            endVote();
            return NightSkipState.NIGHT_NOT_SKIPPED;
        }

        int time = votedTime.addAndGet(1);

        updateTimeLeftBar(world, time, expire);

        if (time >= expire || voteState.size() == world.getPlayerCount()) {
            if (tally()) {
                skipNight();
                return NightSkipState.NIGHT_SKIPPED;
            } else {
                previousWorldNightNoSkip.put(world.getUID(), world.getFullTime());
                endVote();
                return NightSkipState.NIGHT_NOT_SKIPPED;
            }
        }
        return NightSkipState.VOTE_CONTINUES;
    }

    private synchronized void updateTimeLeftBar(World world, int time, int expire) {
        Set<Player> worldPlayers = new HashSet<>(world.getPlayers());
        Set<Player> barPlayers = new HashSet<>(timeLeftBar.getPlayers());
        barPlayers.stream().filter(Predicate.not(worldPlayers::contains)).forEach(timeLeftBar::removePlayer);
        worldPlayers.stream().filter(Predicate.not(barPlayers::contains)).forEach(timeLeftBar::addPlayer);
        timeLeftBar.setTitle(MessageKeys.getSkipBarTitle(
                expire - time,
                expire,
                config.getInteger("no-skip-night-interval", 3))
        );
        timeLeftBar.setProgress(1 - time / (double) expire);
    }

    public void vote(Player voter, boolean skip) {
        World world = getWorld();
        if (world == null) {
            endVote();
            return;
        }

        if (world.getPlayers().contains(voter)) {
            voteState.put(voter.getUniqueId(), skip);
        }
    }

    public void cancelVote(Player player) {
        World world = getWorld();
        if (world == null) {
            endVote();
            return;
        }

        voteState.remove(player.getUniqueId());
    }

    private void skipNight() {
        endVote();

        World world = getWorld();
        if (world == null) {
            return;
        }
        isNightSkipping.add(world.getUID());

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
            if (canSleep(world)) {
                setTimeToDayWithAPI(world);
            }
            isNightSkipping.remove(world.getUID());
            previousWorldNightSkip.put(world.getUID(), world.getFullTime());
        }, 110L);
    }

    private void setTimeToDayWithAPI(World world) {
        if (new TimeSkipEvent(world, TimeSkipEvent.SkipReason.NIGHT_SKIP, 24000 - world.getTime()).callEvent()) {
            if (world.isThundering()) {
                world.setThundering(false);
            }
            if (world.hasStorm()) {
                world.setStorm(false);
            }
            world.setTime(0);
        }
    }

    public Boolean getVoteState(Player voter) {
        return voteState.get(voter.getUniqueId());
    }

    public Map<Boolean, Long> getVoteStates() {
        return voteState.entrySet().stream()
                .collect(Collectors.partitioningBy(Map.Entry::getValue, Collectors.counting()));
    }

    public boolean tally() {
        Map<Boolean, Long> counting = getVoteStates();
        long skipCount = counting.get(true);
        long noSkipCount = counting.get(false);
        return (double) skipCount / (skipCount + noSkipCount) > config.getDouble("skip-ratio", 0.5D);
    }

    public void endVote() {
        voteState.clear();
        votedTime.set(Integer.MIN_VALUE);
        synchronized (timeLeftBar) {
            timeLeftBar.removeAll();
        }
        currentVote.remove(worldUid);
    }
}
