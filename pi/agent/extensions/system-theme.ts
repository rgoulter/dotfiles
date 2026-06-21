/**
 * Sync pi theme with OS appearance using ansi-dark / ansi-light.
 * Kitty's gruvbox palette supplies the underlying ANSI colors.
 *
 * Adapted from pi's mac-system-theme example and leblancfg/pi-ansi-themes.
 */

import { exec } from "node:child_process";
import { promisify } from "node:util";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const execAsync = promisify(exec);

async function detectAppearance(): Promise<"dark" | "light"> {
	if (process.platform === "darwin") {
		try {
			const { stdout } = await execAsync(
				"osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'",
			);
			return stdout.trim() === "true" ? "dark" : "light";
		} catch {
			return "light";
		}
	}

	if (process.platform === "linux") {
		try {
			const { stdout } = await execAsync(
				"gsettings get org.gnome.desktop.interface color-scheme",
			);
			const value = stdout.trim();
			if (value.includes("prefer-dark")) {
				return "dark";
			}
			if (value.includes("prefer-light")) {
				return "light";
			}
		} catch {
			// fall through
		}
	}

	return "dark";
}

function themeForAppearance(mode: "dark" | "light"): string {
	return mode === "dark" ? "ansi-dark" : "ansi-light";
}

export default function (pi: ExtensionAPI) {
	let intervalId: ReturnType<typeof setInterval> | null = null;
	let currentTheme: string | null = null;

	pi.on("session_start", async (_event, ctx) => {
		const appearance = await detectAppearance();
		currentTheme = themeForAppearance(appearance);
		ctx.ui.setTheme(currentTheme);

		intervalId = setInterval(async () => {
			const next = themeForAppearance(await detectAppearance());
			if (next !== currentTheme) {
				currentTheme = next;
				ctx.ui.setTheme(currentTheme);
			}
		}, 2000);
	});

	pi.on("session_shutdown", () => {
		if (intervalId) {
			clearInterval(intervalId);
			intervalId = null;
		}
	});
}