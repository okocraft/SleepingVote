package net.okocraft.sleepingvote;

import dev.siroshun.mcmsgdef.directory.DirectorySource;
import dev.siroshun.mcmsgdef.directory.MessageProcessors;
import dev.siroshun.mcmsgdef.file.PropertiesFile;
import net.kyori.adventure.key.Key;
import org.bukkit.command.PluginCommand;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

public final class SleepingVotePlugin extends JavaPlugin {

    private SleepingVoteListener eventListener;

    @Override
    public void onLoad() {
        saveDefaultConfig();
        reloadConfig();

        try {
            loadMessages();
        } catch (IOException e) {
            getSLF4JLogger().error("Could not load messages.", e);
        }
    }

    @Override
    public void onEnable() {
        SleepingVotes.onPluginEnabled(this);

        eventListener = new SleepingVoteListener(this);
        getServer().getPluginManager().registerEvents(eventListener, this);
        PluginCommand command = Objects.requireNonNull(getCommand("sleepingvote"));
        SleepingVoteCommand commandExecutor = new SleepingVoteCommand(this);
        command.setExecutor(commandExecutor);
        command.setTabCompleter(commandExecutor);
    }

    @Override
    public void onDisable() {
        if (eventListener != null) {
            eventListener.onPluginDisable();
        }
        SleepingVotes.onPluginDisabled();
    }

    public boolean reload() {
        reloadConfig();

        try {
            loadMessages();
        } catch (IOException e) {
            getSLF4JLogger().error("Could not load messages.", e);
            return false;
        }
        return true;
    }

    private void loadMessages() throws IOException {
        DirectorySource.propertiesFiles(getDataFolder().toPath().resolve("languages"))
                .defaultLocale(Locale.ENGLISH, Locale.JAPANESE)
                .primaryLocale(Locale.ENGLISH)
                .messageProcessor(MessageProcessors.appendMissingMessagesToPropertiesFile(this::loadDefaultMessageMap))
                .loadAndRegister(Key.key("sleepingvote", "languages"));
    }

    private @Nullable Map<String, String> loadDefaultMessageMap(@NotNull Locale locale) throws IOException {
        if (locale.equals(Locale.ENGLISH)) {
            return MessageKeys.defaultMessages();
        } else {
            try (var input = getResource(locale + ".properties")) {
                return input != null ? PropertiesFile.load(input) : null;
            }
        }
    }
}
