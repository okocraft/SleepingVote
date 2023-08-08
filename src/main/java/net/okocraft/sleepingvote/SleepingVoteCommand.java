package net.okocraft.sleepingvote;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.entity.Player;
import org.bukkit.util.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class SleepingVoteCommand implements CommandExecutor, TabCompleter {

    private final SleepingVotePlugin plugin;

    SleepingVoteCommand(SleepingVotePlugin plugin) {
        this.plugin = plugin;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull Command command, @NotNull String label,
                             @NotNull String[] args) {
        if (!(sender instanceof Player player)) {
            return true;
        }

        if (!SleepingVotes.canSleep(player.getWorld())) {
            player.sendMessage(MessageKeys.ITS_NOT_NIGHT);
            return true;
        }

        // sv skip
        if (args.length == 0 || "skip".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(player.getWorld())) {
                player.getWorld().getPlayers().forEach(p -> p.getScheduler().run(
                        plugin,
                        t -> p.sendMessage(MessageKeys.START_SLEEPING_VOTE),
                        null));
                player.sendMessage(MessageKeys.TO_CANCEL_VOTE);
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(player.getWorld());
            if (Boolean.TRUE.equals(vote.getVoteState(player))) {
                player.sendMessage(MessageKeys.YOU_ALREADY_VOTED);
                return true;
            }

            vote.vote(player, true);
            player.sendActionBar(MessageKeys.PLAYER_VOTED.apply(player));
            return true;
        }

        // sv noskip
        if ("noskip".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(player.getWorld())) {
                player.sendMessage(MessageKeys.VOTE_NOT_STARTED);
                return true;
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(player.getWorld());
            if (Boolean.FALSE.equals(vote.getVoteState(player))) {
                player.sendMessage(MessageKeys.YOU_ALREADY_VOTED_NOSKIP);
                return true;
            }

            vote.vote(player, false);
            player.sendActionBar(MessageKeys.PLAYER_VOTED.apply(player));
            return true;
        }

        // sv cancel
        if ("cancel".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(player.getWorld())) {
                player.sendMessage(MessageKeys.VOTE_NOT_STARTED);
                return true;
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(player.getWorld());
            if (vote.getVoteState(player) == null) {
                player.sendMessage(MessageKeys.YOU_HAVE_NOT_VOTED_YET);
                return true;
            }

            vote.cancelVote(player);
            player.sendMessage(MessageKeys.VOTE_CANCELLED);
            return true;
        }

        // sv info
        if ("info".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(player.getWorld())) {
                player.sendMessage(MessageKeys.VOTE_NOT_STARTED);
                return true;
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(player.getWorld());
            Boolean playerState = vote.getVoteState(player);
            if (playerState == null) {
                player.sendMessage(MessageKeys.YOU_DO_NOT_VOTE_NOW);
            } else {
                player.sendMessage(MessageKeys.YOUR_VOTE_IS.apply(playerState ? "skip" : "noskip"));
            }
            Map<Boolean, Long> percentage = vote.getVoteStates();
            player.sendMessage(MessageKeys.SKIP_PERCENTAGE.apply(percentage.get(true)));
            player.sendMessage(MessageKeys.NOSKIP_PERCENTAGE.apply(percentage.get(false)));
            return true;
        }

        player.sendMessage(MessageKeys.UNKNOWN_SUBCOMMAND);
        return true;
    }

    @Override
    public @Nullable List<String> onTabComplete(@NotNull CommandSender sender, @NotNull Command command,
                                                @NotNull String label, @NotNull String[] args) {
        if (args.length == 1) {
            return StringUtil.copyPartialMatches(args[0], List.of("skip", "noskip", "cancel", "info"), new ArrayList<>());
        } else {
            return List.of();
        }
    }
}