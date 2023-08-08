package net.okocraft.sleepingvote;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Objects;
import java.util.jar.JarFile;
import java.util.logging.Level;
import net.kyori.adventure.key.Key;
import com.github.siroshun09.configapi.api.Configuration;
import com.github.siroshun09.configapi.api.util.ResourceUtils;
import com.github.siroshun09.configapi.yaml.YamlConfiguration;
import com.github.siroshun09.translationloader.ConfigurationLoader;
import com.github.siroshun09.translationloader.TranslationLoader;
import com.github.siroshun09.translationloader.directory.TranslationDirectory;
import org.bukkit.command.PluginCommand;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public final class SleepingVotePlugin extends JavaPlugin {

    private final Path jarFile;

    private final TranslationDirectory translationDirectory;

    public SleepingVotePlugin() {
        this.jarFile = Paths.get(getClass().getProtectionDomain().getCodeSource().getLocation().getPath());

        Path pluginDirectory = getDataFolder().toPath();

        this.translationDirectory =
                TranslationDirectory.newBuilder()
                        .setDirectory(pluginDirectory.resolve("languages"))
                        .setKey(Key.key("sleepingvote", "languages"))
                        .setDefaultLocale(Locale.ENGLISH)
                        .onDirectoryCreated(this::saveDefaultLanguages)
                        .setVersion(getPluginMeta().getVersion()) // getPluginMeta never returns null
                        .setTranslationLoaderCreator(this::getBundledTranslation)
                        .build();
    }

    private void saveDefaultLanguages(@NotNull Path directory) throws IOException {
        var english = "en.yml";
        ResourceUtils.copyFromJarIfNotExists(jarFile, english, directory.resolve(english));

        var japanese = "ja_JP.yml";
        ResourceUtils.copyFromJarIfNotExists(jarFile, japanese, directory.resolve(japanese));
    }

    private @Nullable TranslationLoader getBundledTranslation(@NotNull Locale locale) throws IOException {
        var strLocale = locale.toString();

        if (!(strLocale.equals("en") || strLocale.equals("ja_JP"))) {
            return null;
        }

        Configuration source;

        try (var jar = new JarFile(getFile());
             var input = ResourceUtils.getInputStreamFromJar(jar, strLocale + ".yml")) {
            source = YamlConfiguration.loadFromInputStream(input);
        }

        var loader = ConfigurationLoader.create(locale, source);
        loader.load();

        return loader;
    }

    @Override
    public void onLoad() {

        try {
            translationDirectory.load();
        } catch (IOException e) {
            getLogger().log(Level.SEVERE, "Could not load languages", e);
        }
    }

    @Override
    public void onEnable() {
        getServer().getPluginManager().registerEvents(new SleepingVoteListener(this), this);
        PluginCommand command = Objects.requireNonNull(getCommand("sleepingvote"));
        SleepingVoteCommand commandExecutor = new SleepingVoteCommand(this);
        command.setExecutor(commandExecutor);
        command.setTabCompleter(commandExecutor);
    }

    @Override
    public void onDisable() {
        translationDirectory.unload();
    }

}
