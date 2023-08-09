package net.okocraft.sleepingvote;

import org.bukkit.Bukkit;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerBedEnterEvent;
import org.bukkit.event.player.PlayerChangedWorldEvent;
import org.bukkit.event.player.PlayerJoinEvent;

public class SleepingVoteListener implements Listener {

    private final SleepingVotePlugin plugin;

    public SleepingVoteListener(SleepingVotePlugin plugin) {
        this.plugin = plugin;
        Bukkit.getGlobalRegionScheduler().runAtFixedRate(plugin, t -> {
            for (World w : Bukkit.getWorlds()) {
                if (!SleepingVotes.isSleepingVoteStarted(w)) {
                    continue;
                }
                SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(w);

                if (!SleepingVotes.canSleep(w)) {
                    w.getPlayers().forEach(p -> p.getScheduler().run(plugin, t2 -> p.sendMessage(MessageKeys.MORNING_CAME), null));
                    vote.endVote();
                    continue;
                }

                // tally up this vote after 30 seconds.
                if (vote.countTimeOne(30)) {
                    w.getPlayers().forEach(p -> p.getScheduler().run(plugin, t2 -> p.sendMessage(MessageKeys.MORNING_CAME), null));
                }
            }
        }, 1, 20);
    }

    @EventHandler
    private void onPlayerJoin(PlayerJoinEvent event) {
        if (SleepingVotes.isSleepingVoteStarted(event.getPlayer().getWorld())) {
            event.getPlayer().sendActionBar(MessageKeys.VOTE_TO_SKIP_NIGHT);
        }
    }

    @EventHandler
    private void onWorldChange(PlayerChangedWorldEvent event) {
        if (SleepingVotes.isSleepingVoteStarted(event.getFrom())) {
            SleepingVotes.getOrCreateSleepingVotes(event.getFrom()).cancelVote(event.getPlayer());
            event.getPlayer().sendMessage(MessageKeys.VOTE_CANCELLED);
        }

        if (SleepingVotes.isSleepingVoteStarted(event.getPlayer().getWorld())) {
            event.getPlayer().sendActionBar(MessageKeys.VOTE_TO_SKIP_NIGHT);
        }
    }

    @EventHandler
    private void onSleeping(PlayerBedEnterEvent event) {
        if (event.getBedEnterResult() != PlayerBedEnterEvent.BedEnterResult.OK) {
            return;
        }

        Player player = event.getPlayer();
        if (!SleepingVotes.isSleepingVoteStarted(player.getWorld())) {
            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(player.getWorld());
            vote.vote(player, true);
            player.getWorld().getPlayers().forEach(p -> p.getScheduler().run(
                    plugin,
                    t -> p.sendMessage(MessageKeys.START_SLEEPING_VOTE),
                    null));
            player.sendMessage(MessageKeys.TO_CANCEL_VOTE);
        } else {
            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(player.getWorld());
            Boolean voteState = vote.getVoteState(player);
            if (voteState == null) {
                vote.vote(player, true);
                player.sendActionBar(MessageKeys.PLAYER_VOTED.apply(player));
            } else {
                event.setCancelled(true);
                vote.cancelVote(player);
                player.sendMessage(MessageKeys.VOTE_CANCELLED);            }
        }
    }
}
