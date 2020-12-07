#!/usr/bin/env swift
// Compile with:
// swiftc dark-mode-notifier.swift -o dark-mode-notifier

import Cocoa

@discardableResult
func shell(_ args: String...) -> Int32 {
    let task = Process()
    task.launchPath = "/usr/bin/env"
    task.arguments = args
    task.launch()
    task.waitUntilExit()
    return task.terminationStatus
}

func updateEmacsTheme() {
    let isDark = UserDefaults.standard.string(forKey: "AppleInterfaceStyle") == "Dark"
    let emacsen = NSRunningApplication.runningApplications(
        withBundleIdentifier: "org.gnu.Emacs")

    if emacsen.count > 0 {
        print("Notifying Emacs that macOS Dark Mode is:", isDark)
        shell("emacsclient", "-e", isDark ? "(fiat-nox)" : "(fiat-lux)")
        print()
    }
}

updateEmacsTheme()

DistributedNotificationCenter.default.addObserver(
    forName: Notification.Name("AppleInterfaceThemeChangedNotification"),
    object: nil,
    queue: nil) { (notification) in
        updateEmacsTheme()
}

// Enter cocoa run loop and start listening for notifyd events
NSApplication.shared.run()
