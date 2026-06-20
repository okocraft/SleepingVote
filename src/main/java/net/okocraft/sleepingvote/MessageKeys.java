package net.okocraft.sleepingvote;

import dev.siroshun.mcmsgdef.DefaultMessageDefiner;
import dev.siroshun.mcmsgdef.MessageKey;
import dev.siroshun.mcmsgdef.Placeholder;
import net.kyori.adventure.text.minimessage.translation.Argument;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.UnmodifiableView;

import java.util.Map;

public final class MessageKeys {

    private static final DefaultMessageDefiner DEFINER = DefaultMessageDefiner.create();

    private static final Placeholder<String> PLAYER_PLACEHOLDER = player -> Argument.string("player", player);
    private static final Placeholder<String> STATE_PLACEHOLDER = state -> Argument.string("state", state);
    private static final Placeholder<Long> COUNT_PLACEHOLDER = count -> Argument.numeric("count", count);
    private static final Placeholder<Integer> TIME_LEFT_PLACEHOLDER = timeLeft -> Argument.numeric("time_left", timeLeft);
    private static final Placeholder<Integer> EXPIRE_PLACEHOLDER = expire -> Argument.numeric("expire", expire);
    private static final Placeholder<Integer> INTERVAL_PLACEHOLDER = interval -> Argument.numeric("interval", interval);

    public static final MessageKey MORNING_CAME = DEFINER.define("morning-came", "<aqua>The morning comes!");
    public static final MessageKey NIGHT_NOT_SKIPPED = DEFINER.define("night-not-skipped", "<aqua>This night is not skipped!");
    public static final MessageKey VOTE_NOT_STARTED = DEFINER.define("vote-not-started", "<aqua>Sleeping vote has not started.");
    public static final MessageKey ITS_NOT_NIGHT = DEFINER.define("its-not-night", "<aqua>You cannot use sleeping vote when it's not night.");
    public static final MessageKey VOTE_CANCELLED = DEFINER.define("vote-cancelled", "<aqua>Your vote has been cancelled.");
    public static final MessageKey TO_CANCEL_VOTE = DEFINER.define("to-cancel-vote", "<aqua>To cancel vote click bed or /sv cancel");
    public static final MessageKey VOTE_TO_SKIP_NIGHT = DEFINER.define("vote-to-skip-night", "<aqua>To skip night, you can vote via entering bed or /sv skip");
    public static final MessageKey.Arg1<String> PLAYER_VOTED = DEFINER.define("player-voted", "<aqua><player> has voted.").with(PLAYER_PLACEHOLDER);
    public static final MessageKey START_SLEEPING_VOTE = DEFINER.define("start-sleeping-vote", "<aqua>Sleeping vote has been started.");
    public static final MessageKey YOU_ALREADY_VOTED = DEFINER.define("you-already-voted", "<aqua>You have already voted skip.");
    public static final MessageKey YOU_ALREADY_VOTED_NOSKIP = DEFINER.define("you-already-voted-noskip", "<aqua>You have already voted to noskip.");
    public static final MessageKey YOU_HAVE_NOT_VOTED_YET = DEFINER.define("you-have-not-voted-yet", "<aqua>You have not voted yet.");
    public static final MessageKey YOU_DO_NOT_VOTE_NOW = DEFINER.define("you-do-not-vote-now", "<aqua>You do not vote now.");
    public static final MessageKey.Arg1<String> YOUR_VOTE_IS = DEFINER.define("your-vote-is", "<aqua>You voted to <state>.").with(STATE_PLACEHOLDER);
    public static final MessageKey.Arg1<Long> SKIP_PERCENTAGE = DEFINER.define("skip-percentage", "<aqua>skip: <count>").with(COUNT_PLACEHOLDER);
    public static final MessageKey.Arg1<Long> NOSKIP_PERCENTAGE = DEFINER.define("noskip-percentage", "<aqua>noskip: <count>").with(COUNT_PLACEHOLDER);
    public static final MessageKey UNKNOWN_SUBCOMMAND = DEFINER.define("unknown-subcommand", "<aqua>Unknown sub-command.");
    public static final MessageKey VOTE_TONIGHT_ENDED = DEFINER.define("vote-tonight-ended", "<aqua>The night skip vote tonight is ended.");
    public static final MessageKey CANNOT_VOTE_TONIGHT = DEFINER.define("cannot-vote-tonight", "<aqua>Cannot vote tonight.");
    public static final MessageKey RELOADED = DEFINER.define("reloaded", "<aqua>Configuration file is reloaded");
    public static final MessageKey.Arg2<Integer, Integer> SKIP_TIME_BAR_TITLE = DEFINER.define("skip-time-bar-title", "<aqua>night skip? /sv skip or /sv noskip (<time_left>/<expire>s)").with(TIME_LEFT_PLACEHOLDER, EXPIRE_PLACEHOLDER);
    public static final MessageKey.Arg3<Integer, Integer, Integer> SKIP_TIME_BAR_TITLE_WITH_INTERVAL = DEFINER.define("skip-time-bar-title-with-interval", "<aqua>night skip? /sv skip or /sv noskip (<time_left>/<expire>s) cannot skip <interval> days later").with(TIME_LEFT_PLACEHOLDER, EXPIRE_PLACEHOLDER, INTERVAL_PLACEHOLDER);
    public static final MessageKey.Arg1<Integer> NEXT_NO_SKIP_NIGHT = DEFINER.define("next-no-skip-night", "<aqua>next unskippable night : <interval> days later").with(INTERVAL_PLACEHOLDER);

    @Contract(pure = true)
    public static @NotNull @UnmodifiableView Map<String, String> defaultMessages() {
        return DEFINER.getCollectedMessages();
    }

    private MessageKeys() {
        throw new UnsupportedOperationException();
    }
}
