package net.okocraft.sleepingvote;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.kyori.adventure.translation.GlobalTranslator;
import org.bukkit.Bukkit;
import org.bukkit.boss.BarColor;
import org.bukkit.boss.BarStyle;
import org.bukkit.boss.BossBar;
import org.bukkit.entity.Player;

public class I18nBossBar {

    private final Map<Locale, BossBar> languageBossBar = new ConcurrentHashMap<>();

    private Component title;
    private BarColor color;
    private BarStyle style;

    public I18nBossBar(Component title, BarColor color, BarStyle style) {
        this.title = title;
        this.color = color;
        this.style = style;
    }

    public void removePlayer(Player player) {
        languageBossBar.values().forEach(b -> b.removePlayer(player));
    }

    public void addPlayer(Player player) {
        languageBossBar.computeIfAbsent(player.locale(), l -> Bukkit.createBossBar(
                LegacyComponentSerializer.legacySection().serialize(GlobalTranslator.render(title, player.locale())),
                color,
                style
        )).addPlayer(player);
    }

    public void setTitle(Component title) {
        this.title = title;
        languageBossBar.forEach((l, b) -> b.setTitle(
                LegacyComponentSerializer.legacySection().serialize(GlobalTranslator.render(title, l))
        ));
    }

    public void setProgress(double progress) {
        languageBossBar.values().forEach(b -> b.setProgress(progress));
    }

    public void removeAll() {
        languageBossBar.values().forEach(BossBar::removeAll);
        languageBossBar.clear();
    }

    public List<Player> getPlayers() {
        return languageBossBar.values().stream().flatMap(b -> b.getPlayers().stream()).toList();
    }
}
