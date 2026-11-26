"""Exercise developer MCP stdio with messages from the published revision."""
import json
import os
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[2]
DEV = Path(os.environ.get("RUPAM_DEV_TEST_BIN", ROOT / "_build/default/dev/rupam_dev.exe")).resolve()
VERSION = "2026-07-28"
VERSION_KEY = "io.modelcontextprotocol/protocolVersion"


def request(method, identifier, version=VERSION, **params):
    params["_meta"] = {
        VERSION_KEY: version,
        "io.modelcontextprotocol/clientCapabilities": {},
    }
    return {"jsonrpc": "2.0", "id": identifier, "method": method, "params": params}


class DeveloperMcp(unittest.TestCase):
    def exchange(self, *messages):
        payload = "".join(
            (message if isinstance(message, str) else json.dumps(message)) + "\n"
            for message in messages
        )
        process = subprocess.run(
            [DEV, "mcp"], input=payload, capture_output=True,
            cwd=ROOT, encoding="utf-8", timeout=30,
        )
        self.assertEqual(process.returncode, 0, process.stderr)
        return [json.loads(line) for line in process.stdout.splitlines()]

    def test_discovery_and_version_are_per_request(self):
        notification = {"jsonrpc": "2.0", "method": "notifications/example"}
        replies = self.exchange(
            request("server/discover", "discovery"),
            notification,
            request("tools/list", 2, "2025-11-25"),
            request("tools/list", 3),
            "{invalid",
            request("ping", 4),
        )
        self.assertEqual([reply["id"] for reply in replies], ["discovery", 2, 3, None, 4])
        discover = replies[0]["result"]
        self.assertEqual(discover["resultType"], "complete")
        self.assertEqual(discover["supportedVersions"], [VERSION])
        self.assertIn("tools", discover["capabilities"])
        self.assertEqual(replies[1]["error"]["code"], -32022)
        self.assertEqual(replies[1]["error"]["data"]["supported"], [VERSION])
        self.assertEqual(replies[3]["error"]["code"], -32700)
        tools = {tool["name"]: tool["inputSchema"] for tool in replies[2]["result"]["tools"]}
        self.assertEqual(
            set(tools), {"check", "tokens", "syntax", "typed", "transform_ir",
                         "code", "evaluate_reference", "conformance", "describe"}
        )
        self.assertEqual(tools["code"]["properties"]["target"]["enum"], ["r-dplyr", "python-polars"])

    def test_compiler_tools_and_structured_diagnostics(self):
        def call(identifier, name, **arguments):
            return request("tools/call", identifier, name=name, arguments=arguments)

        replies = self.exchange(
            call(1, "evaluate_reference", source="let square(x) = x * x\nsquare(3)"),
            call(2, "transform_ir", source="let x = 3"),
            call(3, "check", source="unbound_name"),
            call(4, "code", source="3", target="other"),
            call(5, "conformance", id="C01"),
            call(6, "describe", topic="types"),
        )

        def value(i):
            return replies[i]["result"]["structuredContent"]["value"]

        self.assertEqual(value(0)["shown"], [{"type": "Int", "value": "9"}])
        self.assertIn("(Define", value(1))
        self.assertTrue(replies[2]["result"]["isError"])
        self.assertEqual(value(2)["code"], "unknown-name")
        self.assertIn("span", value(2))
        self.assertEqual(replies[3]["error"]["code"], -32602)
        self.assertEqual(value(4)["status"], "passed")
        self.assertTrue(value(5).startswith("# Types"))

if __name__ == "__main__":
    unittest.main()
