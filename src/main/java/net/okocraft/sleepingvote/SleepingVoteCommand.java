package net.okocraft.sleepingvote;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.bukkit.World;
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

        World world = player.getWorld();

        if (!SleepingVotes.canSleep(world)) {
            player.sendMessage(MessageKeys.ITS_NOT_NIGHT);
            return true;
        }

        if (SleepingVotes.isVoteEnded(world)) {
            player.sendMessage(MessageKeys.VOTE_TONIGHT_ENDED);
            return true;
        }

        if (SleepingVotes.getPreviousNightSkip(world) / 24000
                + plugin.getConfiguration().getInteger("no-skip-night-interval", 3) == world.getFullTime() / 24000) {
            player.sendMessage(MessageKeys.CANNOT_VOTE_TONIGHT);
            return true;
        }

        // sv skip
        if (args.length == 0 || "skip".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(world)) {
                world.getPlayers().forEach(p -> p.getScheduler().run(
                        plugin,
                        t -> p.sendMessage(MessageKeys.START_SLEEPING_VOTE),
                        null));
                player.sendMessage(MessageKeys.TO_CANCEL_VOTE);
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(world);
            if (Boolean.TRUE.equals(vote.getVoteState(player))) {
                player.sendMessage(MessageKeys.YOU_ALREADY_VOTED);
                return true;
            }

            vote.vote(player, true);
            world.getPlayers().forEach(p -> p.getScheduler().run(
                    plugin,
                    t -> p.sendActionBar(MessageKeys.PLAYER_VOTED.apply(player)),
                    null));
            return true;
        }

        // sv noskip
        if ("noskip".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(world)) {
                player.sendMessage(MessageKeys.VOTE_NOT_STARTED);
                return true;
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(world);
            if (Boolean.FALSE.equals(vote.getVoteState(player))) {
                player.sendMessage(MessageKeys.YOU_ALREADY_VOTED_NOSKIP);
                return true;
            }

            vote.vote(player, false);
            world.getPlayers().forEach(p -> p.getScheduler().run(
                    plugin,
                    t -> p.sendActionBar(MessageKeys.PLAYER_VOTED.apply(player)),
                    null));
            return true;
        }

        // sv cancel
        if ("cancel".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(world)) {
                player.sendMessage(MessageKeys.VOTE_NOT_STARTED);
                return true;
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(world);
            if (vote.getVoteState(player) == null) {
                player.sendMessage(MessageKeys.YOU_HAVE_NOT_VOTED_YET);
                return true;
            }

            vote.cancelVote(player);
            world.getPlayers().forEach(p -> p.getScheduler().run(
                    plugin,
                    t -> p.sendActionBar(MessageKeys.VOTE_CANCELLED),
                    null));
            return true;
        }

        // sv info
        if ("info".startsWith(args[0].toLowerCase())) {
            if (!SleepingVotes.isSleepingVoteStarted(world)) {
                player.sendMessage(MessageKeys.VOTE_NOT_STARTED);
                return true;
            }

            SleepingVotes vote = SleepingVotes.getOrCreateSleepingVotes(world);
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

        if (player.hasPermission("sleepingvote.admin")) {
            if ("reload".startsWith(args[0].toLowerCase())) {
                try {
                    plugin.getConfiguration().reload();
                    plugin.getTranslationDirectory().load();
                    player.sendMessage(MessageKeys.RELOADED);
                } catch (IOException e) {
                    e.printStackTrace();
                }
                return true;
            }
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
