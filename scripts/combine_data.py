#!/usr/bin/env python3
"""Combine episodic choice task CSV files into one analysis CSV per task version.

Each task version's raw participant CSVs live in their own subdirectory of
data/raw/ (e.g. data/raw/mixed_memorability/, data/raw/matched_memorability/).
For every such subdirectory, writes two files, keeping only choice-trial and
attention-check rows (plus rating rows for the food-choice task):
  - data/episodic_choice_data-<version>.csv           included participants
  - data/excluded/episodic_choice_data-<version>.csv  excluded participants
The food-choice task has one raw subdirectory per version (data/raw/food_choice/replication_gap23/,
.../replication_gap8/), each combined separately into food_choice_data_<gap>.csv.
Exclusions are decided by the criteria in count_participants.py. Both files are
rebuilt from the raw data on every run, since exclusions (e.g. the food-choice
task's sample-relative ones) can change as participants are added.

Usage:
    python scripts/combine_data.py
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

from count_participants import FOOD_CHOICE_TRIALS, find_excluded_food, find_excluded_task, food_data_filename


SCRIPT_DIR = Path(__file__).resolve().parent
TASK_DIR = SCRIPT_DIR.parent
DATA_DIR = TASK_DIR / "data"
RAW_DIR = DATA_DIR / "raw"
EXCLUDED_DIR = DATA_DIR / "excluded"

KEEPCOLS = [
    "experiment_id",
    "participant_id",
    "is_choice_trial",
    "is_attention_check",
    "rt",
    "response",
    "correct_key",
    "correct",
    "trial_number",
    "block_index",
    "old_trial",
    # ── old-task columns ────────────────────────────────────────
    "memorability_bin",
    "encoding_trial",
    "delay",
    "old_side",
    "old_value",
    "left_is_old",
    "right_is_old",
    "repeat_source_was_chosen",
    "old_chosen",
    # ── shared columns ──────────────────────────────────────────
    "left_image_name",
    "left_image_path",
    "left_memorability",
    "left_value",
    "right_image_name",
    "right_image_path",
    "right_memorability",
    "right_value",
    "chosen_side",
    "chosen_image_name",
    "chosen_image_path",
    "chosen_value",
    "reward",
    "response_key",
    "choice_missed",
    "optimal_old_choice",
    "optimal_choice",
    "final_bonus",
    # ── mixed-memorability columns ──────────────────────────────
    "enc_type",
    "shared_value",
    "chosen_mem_bin",
    "ret_type",
    "left_mem_bin",
    "right_mem_bin",
    "left_is_high",
    "h_value",
    "l_value",
    "delay_h",
    "delay_l",
    "source_hh_trial_number",
    "source_ll_trial_number",
    "hh_source_chosen",
    "ll_source_chosen",
    # ── matched-memorability columns ─────────────────────────────
    "value1_source_trial_number",
    "value0_source_trial_number",
    "delay_value1",
    "delay_value0",
    "left_is_value1",
    "value1_source_chosen",
    "value0_source_chosen",
    # ── direct-value-memory-test columns ──────────────────────────
    "is_recognition_trial",
    "recognition_correct",
    "is_value_test_trial",
    "old_image_name",
    "old_image_path",
    "value_test_response",
    "value_test_missed",
    "value_test_correct",
]

# the food-choice task (Experiment 5) is a different paradigm with its own file and columns
FOOD_KEEPCOLS = [
    "experiment_id",
    "participant_id",
    "phase",
    "is_rating_trial",
    "is_choice_trial",
    "is_attention_check",
    "rt",
    "response",
    "response_key",
    "correct_key",
    "correct",
    # ── rating trials ────────────────────────────────────────────
    "rating_trial_number",
    "image_name",
    "memorability",
    "mem_group",
    "rating",
    # ── choice trials ────────────────────────────────────────────
    "trial_number",
    "pair_type",
    "mem_pair_type",
    "left_image_name",
    "left_image_path",
    "left_memorability",
    "left_mem_group",
    "left_value",
    "left_value_rank",
    "left_mem_group_value_rank",
    "right_image_name",
    "right_image_path",
    "right_memorability",
    "right_mem_group",
    "right_value",
    "right_value_rank",
    "right_mem_group_value_rank",
    "chosen_side",
    "chosen_image_name",
    "chosen_value",
    "distant_gap",
    "chose_right",
    "choice_missed",
    "too_fast",
]


def is_food(version_dir: Path) -> bool:
    return version_dir.parent.name == "food_choice"


def keepcols_for(version_dir: Path) -> list[str]:
    return FOOD_KEEPCOLS if is_food(version_dir) else KEEPCOLS


def truthy(series: pd.Series) -> pd.Series:
    return series.astype("string").str.strip().str.lower().isin(["true", "1", "yes"])


def get_final_bonus(df: pd.DataFrame) -> object:
    if "final_bonus" not in df:
        return pd.NA

    if "is_summary" in df:
        summary = df[truthy(df["is_summary"])]
    else:
        summary = pd.DataFrame()

    if summary.empty:
        summary = df[df["final_bonus"].notna()]
    if summary.empty:
        return pd.NA

    return summary.iloc[-1].get("final_bonus", pd.NA)


def recode_direct_misses_as_missing(df: pd.DataFrame) -> pd.DataFrame:
    """Missed recognition/value-report trials in the direct task were
    originally logged as 0 (an "incorrect" score) rather than missing, which
    silently zeroes out anyone whose responses weren't registering (a bug we
    found in the value-report screen for some sessions) rather than excluding
    them. task.js now writes null directly for new data; recode existing
    already-collected data the same way so it doesn't count a timeout as a
    wrong answer."""
    if "choice_missed" in df and "recognition_correct" in df:
        missed = truthy(df["choice_missed"])
        df.loc[missed, "recognition_correct"] = pd.NA
    if "value_test_missed" in df and "value_test_correct" in df:
        missed = pd.to_numeric(df["value_test_missed"], errors="coerce") == 1
        df.loc[missed, "value_test_correct"] = pd.NA
    return df


def load_csv(csv_path: Path) -> pd.DataFrame:
    df = pd.read_csv(csv_path)

    if "prolific_id" in df:
        df["participant_id"] = df["prolific_id"]

    final_bonus = get_final_bonus(df)

    is_choice = truthy(df["is_choice_trial"]) if "is_choice_trial" in df else pd.Series(False, index=df.index)
    is_attention = truthy(df["is_attention_check"]) if "is_attention_check" in df else pd.Series(False, index=df.index)
    # food-choice task: rating rows are kept too (values are z-scored over each participant's ratings)
    is_rating = truthy(df["is_rating_trial"]) if "is_rating_trial" in df else pd.Series(False, index=df.index)
    df = df[is_choice | is_attention | is_rating].copy()
    df["final_bonus"] = final_bonus
    df["correct"] = compute_attention_check_correct(df)
    df["optimal_old_choice"] = compute_optimal_old_choice(df)
    df = recode_direct_misses_as_missing(df)

    return df[[column for column in dict.fromkeys(KEEPCOLS + FOOD_KEEPCOLS) if column in df]]


def compute_attention_check_correct(df: pd.DataFrame) -> pd.Series:
    correct = df["correct"].copy() if "correct" in df else pd.Series(pd.NA, index=df.index)
    if "correct_key" not in df:
        return correct

    is_attention = truthy(df["is_attention_check"]) if "is_attention_check" in df else pd.Series(False, index=df.index)
    response_key = df["response_key"] if "response_key" in df else pd.Series(pd.NA, index=df.index)
    response = df["response"] if "response" in df else pd.Series(pd.NA, index=df.index)
    pressed_key = response_key.fillna(response)

    # no key pressed (a timed-out check, food-choice task) counts as incorrect
    correct.loc[is_attention] = (
        pressed_key.loc[is_attention].astype("string").str.lower()
        == df.loc[is_attention, "correct_key"].astype("string").str.lower()
    ).fillna(False)
    return correct


def compute_optimal_old_choice(df: pd.DataFrame) -> pd.Series:
    # Mixed-memorability task writes optimal_old_choice as 0/1/null.
    # Older pilots wrote it as "high"/"low"/null strings; normalise those to 0/1
    # using the optimal_choice column (already 0/1) that was always present.
    if "optimal_old_choice" in df:
        existing = df["optimal_old_choice"].dropna()
        if len(existing) > 0:
            as_numeric = pd.to_numeric(existing, errors="coerce")
            if as_numeric.notna().all():
                # Already numeric (0/1) — keep as-is
                return pd.to_numeric(df["optimal_old_choice"], errors="coerce")
            else:
                # Legacy string format ("high"/"low") — replace with optimal_choice (0/1)
                if "optimal_choice" in df:
                    return pd.to_numeric(df["optimal_choice"], errors="coerce")
                return df["optimal_old_choice"]  # fallback: keep strings if no numeric version

    # Old-task format: recompute from old_value and old_chosen
    if "old_value" not in df or "old_chosen" not in df:
        return pd.Series(pd.NA, index=df.index, dtype="Float64")

    old_value = pd.to_numeric(df["old_value"], errors="coerce")
    old_chosen = pd.to_numeric(df["old_chosen"], errors="coerce")
    is_old_choice = truthy(df["is_choice_trial"]) & old_value.notna() & old_chosen.notna()

    optimal = pd.Series(pd.NA, index=df.index, dtype="Float64")
    chose_old_when_high = (old_value > 0.5) & (old_chosen == 1)
    avoided_old_when_low = (old_value < 0.5) & (old_chosen == 0)
    non_tie_old = is_old_choice & (old_value != 0.5)
    optimal.loc[non_tie_old] = (chose_old_when_high | avoided_old_when_low).loc[non_tie_old].astype(int)
    return optimal


def combine_data(version_dir: Path) -> pd.DataFrame:
    frames = [load_csv(csv_path) for csv_path in sorted(version_dir.glob("*.csv"))]

    if not frames:
        return pd.DataFrame(columns=keepcols_for(version_dir))

    combined = pd.concat(frames, ignore_index=True, sort=False)
    return combined.reindex(columns=keepcols_for(version_dir))


def version_dirs() -> list[Path]:
    dirs = sorted(p for p in RAW_DIR.iterdir() if p.is_dir() and not p.name.startswith("."))
    # the food-choice task's versions are subdirectories of data/raw/food_choice/
    return [d for p in dirs for d in ([RAW_DIR / "food_choice" / v for v in FOOD_CHOICE_TRIALS]
                                      if p.name == "food_choice" else [p])]


def output_filename_for(version_dir: Path) -> str:
    # The main task predates the per-version naming convention; keep its
    # combined CSV at the unsuffixed legacy filename.
    if version_dir.name == "main":
        return "episodic_choice_data.csv"
    if is_food(version_dir):
        return food_data_filename(version_dir.name)
    return f"episodic_choice_data-{version_dir.name}.csv"


def combine_version(version_dir: Path) -> None:
    filename = output_filename_for(version_dir)
    combined = combine_data(version_dir)

    if combined.empty:
        print(f"{version_dir.name}: no data yet, skipping")
        return
    if is_food(version_dir):
        excluded = find_excluded_food(combined, FOOD_CHOICE_TRIALS[version_dir.name])
    else:
        excluded = find_excluded_task(combined, filename)

    is_excluded = combined["participant_id"].astype(str).isin({str(pid) for pid in excluded})
    EXCLUDED_DIR.mkdir(exist_ok=True)
    combined[~is_excluded].to_csv(DATA_DIR / filename, index=False)
    combined[is_excluded].to_csv(EXCLUDED_DIR / filename, index=False)

    n_included = combined.loc[~is_excluded, "participant_id"].nunique()
    n_excluded = combined.loc[is_excluded, "participant_id"].nunique()
    print(f"{version_dir.name}: wrote {n_included} included participants to data/{filename} "
          f"and {n_excluded} excluded participants to data/excluded/{filename}")


def main() -> int:
    for version_dir in version_dirs():
        combine_version(version_dir)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
