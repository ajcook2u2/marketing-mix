from pathlib import Path
from robyn_data_validation import DataReviewer

PROJECT_ROOT = Path(__file__).resolve().parent
SAMPLE_WEEKLY_DATA_PATH = str(PROJECT_ROOT / "sample_data" / "simulated_data_weekly.csv")
SAMPLE_DAILY_DATA_PATH = str(PROJECT_ROOT / "sample_data" / "simulated_data_daily.csv")
media_vars = ['tv_S', 'radio_S', 'paid_search_S']
extra_vars = ['competitor_sales']
dependent_var = 'revenue'

dr_weekly = DataReviewer(paid_media_vars=media_vars, paid_media_spends=media_vars, extra_vars=extra_vars, dep_var=dependent_var, file_path=SAMPLE_WEEKLY_DATA_PATH, date_frequency="weekly")
dr_weekly.run_review()

dr_daily = DataReviewer(paid_media_vars=media_vars, paid_media_spends=media_vars, extra_vars=extra_vars, dep_var=dependent_var, file_path=SAMPLE_DAILY_DATA_PATH, date_frequency="daily")
dr_daily.run_review()