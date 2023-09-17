package net.okocraft.sleepingvote;

import io.papermc.paper.threadedregions.scheduler.ScheduledTask;
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
    private final ScheduledTask countingTask;

    public SleepingVoteListener(SleepingVotePlugin plugin) {
        this.plugin = plugin;
        this.countingTask = Bukkit.getGlobalRegionScheduler().runAtFixedRate(plugin, t -> {
            for (World w : Bukkit.getWorlds()) {
                SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(w);
                if (!vote.isVoteStarted()) {
                    vote.onTimeIsSetToMorning();
                    continue;
                }

                SkipState state = vote.countTimeOne();
                // tally up this vote after n seconds.
                if (state == SkipState.NIGHT_SKIPPED) {
                    w.getPlayers().forEach(p -> p.getScheduler().run(plugin, t2 -> p.sendMessage(MessageKeys.MORNING_CAME), null));
                } else if (state == SkipState.NIGHT_NOT_SKIPPED) {
                    w.getPlayers().forEach(p -> p.getScheduler().run(plugin, t2 -> p.sendMessage(MessageKeys.NIGHT_NOT_SKIPPED), null));
                }
            }
        }, 1, 20);
    }

    void onPluginDisable() {
        countingTask.cancel();
    }

    @EventHandler
    private void onPlayerJoin(PlayerJoinEvent event) {
        if (SleepingVotes.getOrCreateSleepingVotes(event.getPlayer().getWorld()).isVoteStarted()) {
            event.getPlayer().sendActionBar(MessageKeys.VOTE_TO_SKIP_NIGHT);
        }
    }

    @EventHandler
    private void onWorldChange(PlayerChangedWorldEvent event) {
        SleepingVotes votes = SleepingVotes.getOrCreateSleepingVotes(event.getFrom());
        if (votes.isVoteStarted()) {
            votes.cancelVote(event.getPlayer());
            event.getPlayer().sendMessage(MessageKeys.VOTE_CANCELLED);
        }

        votes = SleepingVotes.getOrCreateSleepingVotes(event.getPlayer().getWorld());
        if (votes.isVoteStarted()) {
            event.getPlayer().sendActionBar(MessageKeys.VOTE_TO_SKIP_NIGHT);
        }
    }

    @EventHandler
    private void onSleeping(PlayerBedEnterEvent event) {
        if (event.getBedEnterResult() != PlayerBedEnterEvent.BedEnterResult.OK) {
            return;
        }

        Player player = event.getPlayer();
        SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(player.getWorld());

        if (vote.canStartVote()) {
            vote.startVote();
            player.getWorld().getPlayers().forEach(p -> p.getScheduler().run(
                    plugin,
                    t -> p.sendMessage(MessageKeys.START_SLEEPING_VOTE),
                    null));
            player.sendMessage(MessageKeys.TO_CANCEL_VOTE);

            vote.vote(player, true);
            player.getWorld().getPlayers().forEach(p -> p.getScheduler().run(
                    plugin,
                    t -> p.sendActionBar(MessageKeys.PLAYER_VOTED.apply(player)),
                    null));
        } else if (vote.isVoteStarted()) {
            if (vote.getVoteState(player) == null) {
                vote.vote(player, true);
                player.getWorld().getPlayers().forEach(p -> p.getScheduler().run(
                        plugin,
                        t -> p.sendActionBar(MessageKeys.PLAYER_VOTED.apply(player)),
                        null));
            } else {
                event.setCancelled(true);
                vote.cancelVote(player);
                player.sendMessage(MessageKeys.VOTE_CANCELLED);            }
        }
    }
}
