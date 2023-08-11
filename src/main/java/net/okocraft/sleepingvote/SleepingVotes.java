package net.okocraft.sleepingvote;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.bukkit.Bukkit;
import org.bukkit.World;
import org.bukkit.boss.BarColor;
import org.bukkit.boss.BarStyle;
import org.bukkit.boss.BossBar;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.world.TimeSkipEvent;

public class SleepingVotes {

    private static final Map<UUID, SleepingVotes> currentVote = new ConcurrentHashMap<>();
    private static final Map<UUID, Boolean> isNightSkipping = new ConcurrentHashMap<>();

    private final Map<UUID, Boolean> voteState = new ConcurrentHashMap<>();
    private final UUID worldUid;
    private final AtomicInteger votedTime = new AtomicInteger(-1);
    private final BossBar timeLeftBar;

    private SleepingVotes(World world) {
        this.worldUid = world.getUID();
        this.timeLeftBar = Bukkit.createBossBar("", BarColor.BLUE, BarStyle.SOLID);
    }

    public static SleepingVotes getOrCreateSleepingVotes(World world) {
        return currentVote.computeIfAbsent(world.getUID(), u -> new SleepingVotes(world));
    }

    public static boolean isSleepingVoteStarted(World world) {
        return currentVote.containsKey(world.getUID());
    }

    public static boolean canSleep(World world) {
        long time = world.getTime();
        return world.isThundering()
                || (world.hasStorm() && 12010 <= time && time < 23992)
                || (12542 <= time && time < 23460);
    }

    public static void onPluginDisabled() {
        for (World world : Bukkit.getWorlds()) {
            if (isSleepingVoteStarted(world)) {
                getOrCreateSleepingVotes(world).endVote();
            }
        }
    }

    public static boolean isNightSkipping(World world) {
        return isNightSkipping.getOrDefault(world.getUID(), false);
    }

    public World getWorld() {
        return Bukkit.getWorld(worldUid);
    }

    public boolean countTimeOne(int expire) {
        World world = getWorld();
        if (world == null) {
            endVote();
            return false;
        }

        int time = votedTime.addAndGet(1);

        updateTimeLeftBar(world, time, expire);

        if (voteState.size() == world.getPlayerCount() && tally()) {
            skipNight();
            return true;
        }

        if (time >= expire) {
            if (tally()) {
                skipNight();
            } else {
                endVote();
            }
            return true;
        }
        return false;
    }

    private synchronized void updateTimeLeftBar(World world, int time, int expire) {
        Set<Player> worldPlayers = new HashSet<>(world.getPlayers());
        Set<Player> barPlayers = new HashSet<>(timeLeftBar.getPlayers());
        barPlayers.stream().filter(Predicate.not(worldPlayers::contains)).forEach(timeLeftBar::removePlayer);
        worldPlayers.stream().filter(Predicate.not(barPlayers::contains)).forEach(timeLeftBar::addPlayer);

        timeLeftBar.setTitle("night skip? /sv skip or /sv noskip (" + (expire - time) + "/" + expire + "s)");
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
        isNightSkipping.put(world.getUID(), true);

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
        return counting.get(true) >= counting.get(false);
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
