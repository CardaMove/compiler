// Vendors the runtime libraries from rosetta-redo (their source of truth) into this repository,
// and checks that the vendored copies have not drifted. See doc/offchain.md, "Vendoring".
//
// Usage (from the repository root):
//   node scripts/sync-runtime-libs.js           copy the libraries and rewrite resources/runtime-libs.lock.json
//   node scripts/sync-runtime-libs.js --check   verify the vendored files against the lock (and against rosetta-redo, if present)
//
// The rosetta-redo checkout is read from $ROSETTA_REDO, defaulting to ../rosetta-redo.
const { spawnSync } = require("child_process");
const crypto = require("crypto");
const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..");
const rosettaRoot = path.resolve(repoRoot, process.env.ROSETTA_REDO || path.join("..", "rosetta-redo"));
const lockPath = path.join(repoRoot, "resources", "runtime-libs.lock.json");
const rosettaUrl = "https://github.com/roveroniandrea/rosetta-redo";

// Each vendored library: where it lives in rosetta-redo, where it is copied here, and which files belong to it
const libraries = [
  {
    name: "aiken-utils",
    source: "aiken-redo/lib/utils",
    destination: "resources/aiken_template/lib/utils",
    include: (file) => file.endsWith(".ak"),
  },
  {
    name: "typescript-lib",
    source: "aiken-redo/mesh/src/lib",
    destination: "resources/typescript_template/src/lib",
    include: () => true,
  },
];

/** Lists the files of a library folder (recursively, `/`-separated, sorted), or `[]` if the folder is missing */
function listFiles(root, include) {
  if (!fs.existsSync(root)) {
    return [];
  }
  const files = [];
  const walk = (dir) => {
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const full = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        walk(full);
      } else {
        const relative = path.relative(root, full).split(path.sep).join("/");
        if (include(relative)) {
          files.push(relative);
        }
      }
    }
  };
  walk(root);
  return files.sort();
}

/** Hashes a file ignoring CRLF vs LF, so that Windows and Unix checkouts produce the same hash */
function hashFile(file) {
  const content = fs.readFileSync(file).toString("binary").replace(/\r\n/g, "\n");
  return crypto.createHash("sha256").update(content, "binary").digest("hex");
}

/** Maps each file of a library folder to its hash */
function hashLibrary(root, include) {
  return Object.fromEntries(listFiles(root, include).map((file) => [file, hashFile(path.join(root, file))]));
}

/** Runs git inside rosetta-redo and returns its trimmed stdout, failing loudly on error */
function git(...args) {
  const result = spawnSync("git", ["-C", rosettaRoot, ...args], { encoding: "utf8" });
  if (result.error || result.status !== 0) {
    throw new Error(`git ${args.join(" ")} failed in ${rosettaRoot}: ${result.stderr || result.error}`);
  }
  return result.stdout.trim();
}

/** Compares two `file -> hash` maps and returns a human-readable list of differences */
function diffHashes(expected, actual) {
  const problems = [];
  for (const file of Object.keys(expected)) {
    if (!(file in actual)) {
      problems.push(`missing: ${file}`);
    } else if (expected[file] !== actual[file]) {
      problems.push(`changed: ${file}`);
    }
  }
  for (const file of Object.keys(actual)) {
    if (!(file in expected)) {
      problems.push(`unexpected: ${file}`);
    }
  }
  return problems;
}

/** Copies every library from rosetta-redo and rewrites the lock file */
function sync() {
  if (!fs.existsSync(path.join(rosettaRoot, ".git"))) {
    throw new Error(`rosetta-redo not found at ${rosettaRoot} (set ROSETTA_REDO)`);
  }
  const dirty = git("status", "--porcelain", "--", ...libraries.map((lib) => lib.source));
  if (dirty) {
    throw new Error(`rosetta-redo has uncommitted changes in the vendored libraries, commit them first:\n${dirty}`);
  }
  const lock = { source: { repository: rosettaUrl, commit: git("rev-parse", "HEAD") }, libraries: {} };

  for (const lib of libraries) {
    const sourceDir = path.join(rosettaRoot, lib.source);
    const destinationDir = path.join(repoRoot, lib.destination);
    // The destination only holds vendored files, so stale ones are removed before copying
    for (const file of listFiles(destinationDir, lib.include)) {
      fs.rmSync(path.join(destinationDir, file));
    }
    for (const file of listFiles(sourceDir, lib.include)) {
      fs.mkdirSync(path.dirname(path.join(destinationDir, file)), { recursive: true });
      fs.copyFileSync(path.join(sourceDir, file), path.join(destinationDir, file));
    }
    const files = hashLibrary(destinationDir, lib.include);
    lock.libraries[lib.name] = { source: lib.source, destination: lib.destination, files };
    console.log(`${lib.name}: ${Object.keys(files).length} files -> ${lib.destination}`);
  }

  fs.writeFileSync(lockPath, JSON.stringify(lock, null, 2) + "\n");
  console.log(`Synced from rosetta-redo ${lock.source.commit}, wrote ${path.relative(repoRoot, lockPath)}`);
}

/** Verifies the vendored files against the lock and, when available, against rosetta-redo */
function check() {
  const lock = JSON.parse(fs.readFileSync(lockPath, "utf8"));
  const rosettaAvailable = fs.existsSync(path.join(rosettaRoot, ".git"));
  let failed = false;

  for (const lib of libraries) {
    const locked = lock.libraries[lib.name];
    if (!locked) {
      console.error(`${lib.name}: missing from the lock, run the sync`);
      failed = true;
      continue;
    }
    const localProblems = diffHashes(locked.files, hashLibrary(path.join(repoRoot, lib.destination), lib.include));
    if (localProblems.length > 0) {
      console.error(`${lib.name}: vendored copy was edited locally (fix it in rosetta-redo, then sync):`);
      localProblems.forEach((p) => console.error(`  ${p}`));
      failed = true;
    }
    if (rosettaAvailable) {
      const upstreamProblems = diffHashes(locked.files, hashLibrary(path.join(rosettaRoot, lib.source), lib.include));
      if (upstreamProblems.length > 0) {
        console.error(`${lib.name}: rosetta-redo differs from the vendored copy (run the sync):`);
        upstreamProblems.forEach((p) => console.error(`  ${p}`));
        failed = true;
      }
    }
  }

  if (!rosettaAvailable) {
    console.log(`rosetta-redo not found at ${rosettaRoot}: only the local copies were checked`);
  }
  if (failed) {
    process.exit(1);
  }
  console.log(`Vendored runtime libraries match the lock (rosetta-redo ${lock.source.commit})`);
}

if (process.argv.includes("--check")) {
  check();
} else {
  sync();
}
