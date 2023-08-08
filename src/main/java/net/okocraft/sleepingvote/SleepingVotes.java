package net.okocraft.sleepingvote;

import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import org.bukkit.Bukkit;
import org.bukkit.World;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.world.TimeSkipEvent;

public class SleepingVotes {

    private static final Map<UUID, SleepingVotes> currentVote = new ConcurrentHashMap<>();

    private final Map<UUID, Boolean> voteState = new ConcurrentHashMap<>();
    private final UUID worldUid;
    private final AtomicInteger votedTime = new AtomicInteger(0);

    private SleepingVotes(World world) {
        this.worldUid = world.getUID();
        currentVote.put(world.getUID(), this);
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

    public World getWorld() {
        return Bukkit.getWorld(worldUid);
    }

    public boolean countTimeOne(int expire) {
        int time = votedTime.addAndGet(1);
        if (time >= expire && tally()) {
            skipNight();
            return true;
        }
        return false;
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

        if (voteState.size() == world.getPlayerCount() && tally()) {
            skipNight();
        }
    }

    public void cancelVote(Player player) {
        World world = getWorld();
        if (world == null) {
            endVote();
            return;
        }

        voteState.remove(player.getUniqueId());

        if (voteState.size() == world.getPlayerCount() && tally()) {
            skipNight();
        }
    }

    private void skipNight() {
        endVote();

        World world = getWorld();
        if (world == null) {
            return;
        }

        Optional<Player> sleeperOptional = world.getPlayers().stream().filter(LivingEntity::isSleeping).findAny();
        if (sleeperOptional.isEmpty()) {
            if (new TimeSkipEvent(world, TimeSkipEvent.SkipReason.NIGHT_SKIP, 24000 - world.getTime()).callEvent()) {
                if (world.isThundering()) {
                    world.setThundering(false);
                }
                if (world.hasStorm()) {
                    world.setStorm(false);
                }
                world.setTime(0);
            }
        } else {
            Player sleeper = sleeperOptional.get();
            SleepingVotePlugin plugin = SleepingVotePlugin.getPlugin(SleepingVotePlugin.class);
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
                        40L
                );
            }
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
        currentVote.remove(worldUid);
    }
}
