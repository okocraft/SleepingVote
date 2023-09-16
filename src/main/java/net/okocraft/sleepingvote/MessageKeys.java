package net.okocraft.sleepingvote;

import java.util.function.Function;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.entity.Player;

public class MessageKeys {

    public static final Component MORNING_CAME = Component.translatable("morning-came").color(NamedTextColor.AQUA);
    public static final Component NIGHT_NOT_SKIPPED = Component.translatable("night-not-skipped").color(NamedTextColor.AQUA);
    public static final Component VOTE_NOT_STARTED = Component.translatable("vote-not-started").color(NamedTextColor.AQUA);
    public static final Component ITS_NOT_NIGHT = Component.translatable("its-not-night").color(NamedTextColor.AQUA);
    public static final Component VOTE_CANCELLED = Component.translatable("vote-cancelled").color(NamedTextColor.AQUA);
    public static final Component TO_CANCEL_VOTE = Component.translatable("to-cancel-vote").color(NamedTextColor.AQUA);
    public static final Component VOTE_TO_SKIP_NIGHT = Component.translatable("vote-to-skip-night").color(NamedTextColor.AQUA);
    public static final Function<Player, Component> PLAYER_VOTED = p ->
            Component.translatable("player-voted").color(NamedTextColor.AQUA).args(Component.text(p.getName()));
    public static final Component START_SLEEPING_VOTE = Component.translatable("start-sleeping-vote").color(NamedTextColor.AQUA);
    public static final Component YOU_ALREADY_VOTED = Component.translatable("you-already-voted").color(NamedTextColor.AQUA);
    public static final Component YOU_ALREADY_VOTED_NOSKIP = Component.translatable("you-already-voted-noskip").color(NamedTextColor.AQUA);
    public static final Component YOU_HAVE_NOT_VOTED_YET = Component.translatable("you-have-not-voted-yet").color(NamedTextColor.AQUA);
    public static final Component YOU_DO_NOT_VOTE_NOW = Component.translatable("you-do-not-vote-now").color(NamedTextColor.AQUA);
    public static final Function<String, Component> YOUR_VOTE_IS = state ->
            Component.translatable("your-vote-is").color(NamedTextColor.AQUA).args(Component.text(state));
    public static final Function<Long, Component> SKIP_PERCENTAGE = count ->
            Component.translatable("skip-percentage").color(NamedTextColor.AQUA).args(Component.text(count));
    public static final Function<Long, Component> NOSKIP_PERCENTAGE = count ->
            Component.translatable("noskip-percentage").color(NamedTextColor.AQUA).args(Component.text(count));
    public static final Component UNKNOWN_SUBCOMMAND = Component.translatable("unknown-subcommand").color(NamedTextColor.AQUA);
    public static final Component VOTE_TONIGHT_ENDED = Component.translatable("vote-tonight-ended").color(NamedTextColor.AQUA);
    public static final Component CANNOT_VOTE_TONIGHT = Component.translatable("cannot-vote-tonight").color(NamedTextColor.AQUA);
    public static final Component RELOADED = Component.translatable("reloaded").color(NamedTextColor.AQUA);
    public static Component getSkipBarTitle(int timeLeft, int expire, int noSkipDayInterval) {
        return Component.translatable("skip-time-bar-title").color(NamedTextColor.AQUA).args(
                Component.text(timeLeft).color(NamedTextColor.AQUA),
                Component.text(expire).color(NamedTextColor.AQUA),
                Component.text(noSkipDayInterval).color(NamedTextColor.AQUA)
        );
    }

}
