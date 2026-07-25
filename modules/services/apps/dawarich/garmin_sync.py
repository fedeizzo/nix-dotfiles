import os
import time
import datetime
import requests
import garth
from garminconnect import Garmin

GARMIN_EMAIL = os.getenv("GARMIN_EMAIL")
GARMIN_PASSWORD = os.getenv("GARMIN_PASSWORD")
GARMIN_TOKENSTORE = os.path.expanduser(os.getenv("GARMIN_TOKENSTORE", "/var/lib/garmin-sync/tokens"))

DAWARICH_HOST = os.getenv("DAWARICH_HOST", "http://127.0.0.1:55224")
DAWARICH_API_KEY = os.getenv("DAWARICH_API_KEY")

DEFAULT_LOOKBACK_DAYS = int(os.getenv("DAYS_TO_SYNC", "7"))
START_DATE_ENV = os.getenv("START_DATE")

DOWNLOAD_DELAY = float(os.getenv("DOWNLOAD_DELAY", "1.5"))
OUTPUT_DIR = os.getenv("OUTPUT_DIR", "/tmp/garmin_gpx")


def get_existing_imported_filenames() -> set[str]:
    url = f"{DAWARICH_HOST.rstrip('/')}/api/v1/imports"
    headers = {"Authorization": f"Bearer {DAWARICH_API_KEY}"}

    try:
        response = requests.get(url, headers=headers, timeout=30)
        response.raise_for_status()
        imports = response.json()

        filenames = set()
        for item in imports:
            name = item.get("filename") or item.get("name")
            if not name and isinstance(item.get("file"), dict):
                name = item["file"].get("filename")
            if name:
                filenames.add(name)

        return filenames
    except Exception as e:
        print(f"[Dawarich API] Warning: Failed to fetch imports list: {e}")
        return set()


def fetch_garmin_gpx_range(start_date_str: str, end_date_str: str) -> list[str]:
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    os.makedirs(GARMIN_TOKENSTORE, exist_ok=True)

    if not GARMIN_EMAIL or not GARMIN_PASSWORD:
        raise ValueError("GARMIN_EMAIL and GARMIN_PASSWORD must be set in environment.")

    gc = Garmin(email=GARMIN_EMAIL, password=GARMIN_PASSWORD)

    # 1. Try to resume session from saved OAuth tokens first
    try:
        garth.resume(GARMIN_TOKENSTORE)
        print(f"[Garmin] Successfully resumed token session from {GARMIN_TOKENSTORE}")
    except Exception as e:
        print(f"[Garmin] Token session resume failed ({e}). Logging in with credentials...")
        try:
            gc.login()
            garth.save(GARMIN_TOKENSTORE)
            print(f"[Garmin] Credential login successful! Session tokens saved to {GARMIN_TOKENSTORE}.")
        except Exception as login_err:
            err_str = str(login_err)
            if "429" in err_str:
                print(
                    "\n[Garmin Error 429] Too Many Requests!\n"
                    "Garmin SSO has temporarily rate-limited password authentication requests for your IP/account.\n"
                    "Garmin SSO rate limits typically reset after 15 to 30 minutes.\n"
                    "-> Please wait 15–30 minutes before running the script again.\n"
                )
            raise

    print(f"[Garmin] Querying activities from {start_date_str} to {end_date_str}...")
    activities = gc.get_activities_by_date(start_date_str, end_date_str)
    print(f"[Garmin] Found {len(activities)} total activities in range.")

    downloaded_files = []

    for act in activities:
        act_id = act["activityId"]
        act_date = act["startTimeLocal"].split()[0]
        filename = f"{act_date}_{act_id}.gpx"
        file_path = os.path.join(OUTPUT_DIR, filename)

        if os.path.exists(file_path):
            downloaded_files.append(file_path)
            continue

        print(f"[Garmin] Downloading activity {act_id} ({act.get('activityName')})...")
        try:
            gpx_data = gc.download_activity(act_id, dl_fmt=gc.ActivityDownloadFormat.GPX)
        except Exception as e:
            print(f"[Garmin] Failed to download activity {act_id}: {e}")
            continue

        if b"<trkpt" not in gpx_data:
            print(f"[Garmin] Skipping activity {act_id} (no GPS trackpoints).")
            continue

        with open(file_path, "wb") as f:
            f.write(gpx_data)

        downloaded_files.append(file_path)
        time.sleep(DOWNLOAD_DELAY)

    return downloaded_files


def upload_to_dawarich_api(gpx_path: str) -> bool:
    url = f"{DAWARICH_HOST.rstrip('/')}/api/v1/imports"
    headers = {"Authorization": f"Bearer {DAWARICH_API_KEY}"}
    filename = os.path.basename(gpx_path)

    with open(gpx_path, "rb") as f:
        files = {"file": (filename, f, "application/gpx+xml")}
        response = requests.post(url, headers=headers, files=files, timeout=60)

    if response.status_code in (200, 201):
        print(f"[Dawarich API] Successfully queued import for {filename}")
        return True
    else:
        print(f"[Dawarich API] Error ({response.status_code}): {response.text}")
        return False


if __name__ == "__main__":
    today = datetime.date.today()
    end_date_str = today.strftime("%Y-%m-%d")

    if START_DATE_ENV:
        start_date_str = START_DATE_ENV
    else:
        start_date_str = (today - datetime.timedelta(days=DEFAULT_LOOKBACK_DAYS)).strftime("%Y-%m-%d")

    print(f"--- Starting Garmin -> Dawarich Sync ---")
    print(f"Range: {start_date_str} to {end_date_str}")

    existing_imports = get_existing_imported_filenames()
    print(f"[Dawarich API] Found {len(existing_imports)} existing imports.")

    gpx_files = fetch_garmin_gpx_range(start_date_str, end_date_str)

    new_uploads = 0
    for gpx_file in gpx_files:
        filename = os.path.basename(gpx_file)

        if filename in existing_imports:
            continue

        if upload_to_dawarich_api(gpx_file):
            new_uploads += 1
            time.sleep(1)

    print(f"--- Sync Finished. Uploaded {new_uploads} new activities. ---")
