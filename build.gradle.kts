plugins {
    alias(libs.plugins.jcommon)
    alias(libs.plugins.bundler)
}

group = "net.okocraft.sleepingvote"
version = "1.0"

jcommon {
    javaVersion = JavaVersion.VERSION_25

    setupPaperRepository()

    commonDependencies {
        compileOnlyApi(libs.paper)
    }
}

bundler {
    copyToRootBuildDirectory("SleepingVote-${project.version}")
    replacePluginVersionForBukkit(project.version)
}
