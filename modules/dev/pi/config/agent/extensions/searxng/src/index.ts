import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

const SEARXNG_URL = "https://search.fedeizzo.dev";

export default function (pi: ExtensionAPI) {
  pi.on("session_start", async (_event, ctx) => {
    ctx.ui.notify("SearXNG search enabled", "info");
  });

  pi.registerTool({
    name: "search_web",
    label: "Search Web",
    description:
      "Search the internet using a self-hosted SearXNG instance. Returns results with titles, URLs, and snippets.",
    promptSnippet: "Search the internet via SearXNG. Returns title, URL, and snippet for each result.",
    promptGuidelines: [
      "Use search_web for up-to-date information, current events, or facts that may not be in training data.",
    ],
    parameters: Type.Object({
      query: Type.String({ description: "Search query string" }),
      categories: Type.Optional(
        Type.String({
          description:
            "Comma-separated categories (general, it, science, images, videos, news, music, files,IT, general)",
          default: "general",
        })
      ),
      number_of_results: Type.Optional(
        Type.Integer({
          description: "Maximum number of results to return (1-20). Defaults to 8.",
          minimum: 1,
          maximum: 20,
          default: 8,
        })
      ),
    }),
    async execute(_toolCallId, params, signal) {
      const query = params.query;
      const categories = params.categories || "general";
      const limit = Math.min(Math.max(params.number_of_results ?? 8, 1), 20);
      const url = new URL("/search", SEARXNG_URL);
      url.searchParams.set("q", query);
      url.searchParams.set("format", "json");
      url.searchParams.set("categories", categories);
      url.searchParams.set("number_of_results", String(limit));

      const response = await fetch(url.toString(), { signal });

      if (!response.ok) {
        return {
          content: [
            {
              type: "text",
              text: `Search failed (HTTP ${response.status}): ${response.statusText}`,
            },
          ],
          details: { query, error: response.statusText },
          isError: true,
        };
      }

      const data = (await response.json()) as {
        query: string;
        number_of_results: number;
        results: Array<{
          url: string;
          title?: string;
          content?: string;
          publishedDate?: string;
          engines: string[];
        }>;
      };

      const results = data.results.slice(0, limit);

      if (results.length === 0) {
        return {
          content: [
            {
              type: "text",
              text: `No results found for "${query}".`,
            },
          ],
          details: { query, total_results: data.number_of_results, result_count: 0 },
        };
      }

      const lines: string[] = [
        `Search results for "${query}" (${results.length} of ${data.number_of_results}):`,
        "",
      ];

      results.forEach((r, i) => {
        lines.push(`${i + 1}. ${r.title || "Untitled"}`);
        lines.push(`   URL: ${r.url}`);
        if (r.content) {
          lines.push(`   Snippet: ${r.content.slice(0, 200)}`);
        }
        if (r.publishedDate) {
          lines.push(`   Date: ${r.publishedDate}`);
        }
        lines.push(`   Source: ${r.engines.join(", ")}`);
        lines.push("");
      });

      return {
        content: [{ type: "text", text: lines.join("\n") }],
        details: { query, total_results: data.number_of_results, result_count: results.length },
        isError: false,
      };
    },
  });
}
