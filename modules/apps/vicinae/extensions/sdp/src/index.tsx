import { Action, ActionPanel, List } from "@vicinae/api";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";

const MOSAIC_URL = "https://mosaic.us1.ddbuild.io/services/details";
const SERVICES_FILE = path.join(os.homedir(), "dd", "synthetics-services.json");

const SERVICES: string[] = JSON.parse(fs.readFileSync(SERVICES_FILE, "utf8"));

export default function Command() {
  return (
    <List searchBarPlaceholder="Search services...">
      {SERVICES.map((service) => (
        <List.Item
          key={service}
          title={service}
          actions={
            <ActionPanel>
              <Action.OpenInBrowser
                title="Open in Mosaic"
                url={`${MOSAIC_URL}?name=${encodeURIComponent(service)}`}
              />
            </ActionPanel>
          }
        />
      ))}
    </List>
  );
}
