plugins {
    java
    id("com.github.johnrengelman.shadow") version "8.1.0"
}

group = "net.okocraft.sleepingvote"
version = "1.0"

repositories {
    mavenCentral()
    maven(url = "https://repo.papermc.io/repository/maven-public/")
}

dependencies {
    compileOnly("io.papermc.paper:paper-api:1.20.1-R0.1-SNAPSHOT")

    implementation("com.github.siroshun09.configapi:configapi-yaml:4.6.4")
    implementation("com.github.siroshun09.translationloader:translationloader:2.0.2")

    testImplementation("org.junit.jupiter:junit-jupiter-api:5.9.0")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine")
}

tasks.build {
    dependsOn(tasks.named("shadowJar"))
}

tasks.processResources {
    filesMatching(listOf("plugin.yml", "en.yml", "ja_JP.yml")) {
        expand("projectVersion" to version)
    }
}

tasks.test {
    useJUnitPlatform()
}

tasks.shadowJar {
    minimize()
    relocate("com.github.siroshun09", "net.okocraft.boxtradestick.lib")
}
