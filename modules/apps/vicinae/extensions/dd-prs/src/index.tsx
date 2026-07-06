import { Action, ActionPanel, List, showToast, Toast } from "@vicinae/api";
import { execFile } from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { useEffect, useState } from "react";
import { promisify } from "util";

const execFileAsync = promisify(execFile);

const DD_DIR = path.join(os.homedir(), "dd");
const PREFIX_MAP_FILE = path.join(DD_DIR, "repo-prefixes.json");

function loadPrefixMap(): Record<string, string> {
  try {
    return JSON.parse(fs.readFileSync(PREFIX_MAP_FILE, "utf8"));
  } catch {
    return {};
  }
}

function prefixFor(repoDir: string): string {
  const name = path.basename(repoDir);
  for (const [key, value] of Object.entries(loadPrefixMap())) {
    if (name.includes(key)) return value;
  }
  return "";
}

function listRepoDirs(): string[] {
  return fs
    .readdirSync(DD_DIR, { withFileTypes: true })
    .filter((entry) => entry.isDirectory())
    .map((entry) => entry.name)
    .sort();
}

type PullRequest = {
  title: string;
  url: string;
};

const EXTRA_PATH_DIRS = ["/opt/homebrew/bin", "/usr/local/bin"];

async function fetchPullRequests(repoDir: string): Promise<PullRequest[]> {
  const { stdout } = await execFileAsync(
    "gh",
    ["pr", "list", "--author", "@me", "--json", "title,url"],
    {
      cwd: path.join(DD_DIR, repoDir),
      env: { ...process.env, PATH: `${EXTRA_PATH_DIRS.join(":")}:${process.env.PATH ?? ""}` },
    },
  );

  return JSON.parse(stdout);
}

function PullRequestList({ repoDir }: { repoDir: string }) {
  const [prs, setPrs] = useState<PullRequest[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const prefix = prefixFor(repoDir);

  useEffect(() => {
    fetchPullRequests(repoDir)
      .then(setPrs)
      .catch((error) => {
        showToast({
          style: Toast.Style.Failure,
          title: "Failed to list pull requests",
          message: error instanceof Error ? error.message : String(error),
        });
      })
      .finally(() => setIsLoading(false));
  }, [repoDir]);

  return (
    <List isLoading={isLoading} searchBarPlaceholder="Search pull requests...">
      {prs.map((pr) => (
        <List.Item
          key={pr.url}
          title={pr.title}
          subtitle={`${prefix} ${pr.title}`}
          actions={
            <ActionPanel>
              <Action.CopyToClipboard title="Copy" content={`${prefix} ${pr.title}\n${pr.url}`} />
            </ActionPanel>
          }
        />
      ))}
    </List>
  );
}

export default function Command() {
  const [repoDirs, setRepoDirs] = useState<string[]>([]);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    try {
      setRepoDirs(listRepoDirs());
    } catch (error) {
      showToast({
        style: Toast.Style.Failure,
        title: "Failed to list ~/dd directories",
        message: error instanceof Error ? error.message : String(error),
      });
    } finally {
      setIsLoading(false);
    }
  }, []);

  return (
    <List isLoading={isLoading} searchBarPlaceholder="Search repositories...">
      {repoDirs.map((repoDir) => (
        <List.Item
          key={repoDir}
          title={repoDir}
          actions={
            <ActionPanel>
              <Action.Push title="Show Pull Requests" target={<PullRequestList repoDir={repoDir} />} />
            </ActionPanel>
          }
        />
      ))}
    </List>
  );
}
