import streamlit as st
import plotly.express as px
import plotly.graph_objects as go

import pandas as pd
import collections
import numpy as np

from pathlib import Path
from datetime import datetime
from datetime import date as dt

_MAX_WIDTH = 1000
_NUM_TO_TIME_SLOT = {
    0: "00:00",
    1: "00:30",
    2: "01:00",
    3: "01:30",
    4: "02:00",
    5: "02:30",
    6: "03:00",
    7: "03:30",
    8: "04:00",
    9: "04:30",
    10: "05:00",
    11: "05:30",
    12: "06:00",
    13: "06:30",
    14: "07:00",
    15: "07:30",
    16: "08:00",
    17: "08:30",
    18: "09:00",
    19: "09:30",
    20: "10:00",
    21: "10:30",
    22: "11:00",
    23: "11:30",
    24: "12:00",
    25: "12:30",
    26: "13:00",
    27: "13:30",
    28: "14:00",
    29: "14:30",
    30: "15:00",
    31: "15:30",
    32: "16:00",
    33: "16:30",
    34: "17:00",
    35: "17:30",
    36: "18:00",
    37: "18:30",
    38: "19:00",
    39: "19:30",
    40: "20:00",
    41: "20:30",
    42: "21:00",
    43: "21:30",
    44: "22:00",
    45: "22:30",
    46: "23:00",
    47: "23:30",
}


#############################
#
# UTILITY FUNCTIONS
#
#############################


def _max_width_(n):
    max_width_str = f"max-width: {n}px;"
    st.markdown(
        f"""
    <style>
    .reportview-container .main .block-container{{
        {max_width_str}
    }}
    </style>
    """,
        unsafe_allow_html=True,
    )


def date_converter(date):
    right_year = 2019
    if isinstance(date, dt):
        return date.strftime("%j")
    else:
        if date > 366:
            year = int(date / 366)
            date -= 366 * year
            right_year += year
        return datetime.strptime(str(date), "%j").strftime(f"%d/%m/{right_year}")


#############################
#
# DRAWING FUNCTIONS
#
#############################


def draw_charts_summary(filteredDatset):
    occurences = dict(collections.Counter(filteredDatset.value.values))
    if np.nan in occurences.keys():
        del occurences[np.nan]

    occurences = (
        pd.DataFrame(occurences, index=["Amount"])
        .transpose()
        .sort_values("Amount", ascending=False)
    )
    occurences["Amount"] = occurences["Amount"].apply(lambda x: x / 2)

    # Bar chart
    st.write(occurences)
    st.bar_chart(occurences)

    # Pie chart
    occurences.reset_index(inplace=True)
    occurences.rename(columns={"index": "Key"}, inplace=True)
    pie_chart = px.pie(occurences, values="Amount", names="Key")
    pie_chart.update_layout(
        autosize=False, width=_MAX_WIDTH, margin=dict(l=20, r=20, t=20, b=20),
    )
    st.plotly_chart(
        pie_chart,
        config={
            "scrollZoom": False,
            "displayModeBar": False,
            "editable": False,
            "showLink": False,
            "displaylogo": False,
        },
    )


def draw_charts_summary_trend(filteredDatset):
    # st.write(filteredDatset)
    filteredDatset.value = filteredDatset.value.astype(str)
    filteredDatset = filteredDatset[filteredDatset.value != "nan"]
    filteredDatset = (
        filteredDatset[["DATA", "value", "variable"]].groupby(["DATA", "value"]).count()
    )
    filteredDatset.reset_index(inplace=True)
    categories = filteredDatset.value.unique()
    datas = filteredDatset.DATA.unique()
    filteredDatset.variable = filteredDatset.variable / 2
    for d in datas:
        cat = filteredDatset[filteredDatset.DATA == d].value.unique()
        diff = np.setdiff1d(categories, cat).tolist()
        diff = pd.concat(
            [
                pd.DataFrame([[d, c, 0]], columns=["DATA", "value", "variable"])
                for c in diff
            ],
            ignore_index=True,
        )
        # filteredDatset = pd.concat([filteredDatset, diff], ignore_index=True)

    filteredDatset.sort_values(by=["DATA"], inplace=True)
    cat_selected = st.multiselect("Select day/s", options=categories,)

    mean_for_day = filteredDatset.groupby(["value"]).agg(["sum"])
    mean_for_day["mean"] = mean_for_day["variable"] / len(datas)
    mean_for_day["percentage"] = (
        mean_for_day["mean"] / mean_for_day["mean"].sum()
    ) * 100
    mean_for_day.sort_values(["percentage"], inplace=True, ascending=False)
    st.write(mean_for_day)

    trend = go.Figure()
    cumulative = go.Figure()
    for c in cat_selected:
        df = filteredDatset[filteredDatset.value == c]
        cumsum = df.variable.cumsum()
        trend.add_trace(
            go.Scatter(
                x=df["DATA"],
                y=df["variable"],
                mode="markers+lines",
                name=c,
                # text=rolling,
            )
        )
        cumulative.add_trace(
            go.Scatter(
                x=df["DATA"],
                y=cumsum,
                mode="lines",
                name=c,
                # text=rolling,
            )
        )
    trend.update_layout(
        autosize=False, width=_MAX_WIDTH, margin=dict(l=20, r=20, t=20, b=20),
    )
    cumulative.update_layout(
        autosize=False, width=_MAX_WIDTH, margin=dict(l=20, r=20, t=20, b=20),
    )
    st.plotly_chart(
        trend,
        config={
            "scrollZoom": False,
            "displayModeBar": False,
            "editable": False,
            "showLink": False,
            "displaylogo": False,
        },
    )
    st.plotly_chart(
        cumulative,
        config={
            "scrollZoom": False,
            "displayModeBar": False,
            "editable": False,
            "showLink": False,
            "displaylogo": False,
        },
    )


def draw_charts_comparison(filteredDatset, type_comparison):
    # occurences = []
    occurences = pd.DataFrame()
    elements_pie_chart = []
    for i, id_comparison in enumerate(filteredDatset.id_comparison.unique()):
        df = filteredDatset[filteredDatset.id_comparison == id_comparison]
        if type_comparison == "range":
            column_name = "range " + str(id_comparison)
        elif type_comparison == "day":
            column_name = "/".join(df.NOME_GIORNO.unique())
        elif type_comparison == "month":
            column_name = "/".join(df.MESE.unique())

        occ = dict(collections.Counter(df.value.values))

        if np.nan in occ.keys():
            del occ[np.nan]

        occ = (
            pd.DataFrame(occ, index=["Amount"])
            .transpose()
            .sort_values("Amount", ascending=False)
        )
        occ["Comparison"] = column_name
        occ["Amount"] = occ["Amount"].apply(lambda x: x / 1)
        occurences = pd.concat([occurences, occ])
        # occ = {k: v / 2 for k, v in occ.items()}
        # occurences.append(occ)
        elements_pie_chart.append(column_name)

    # occurences = (
    #     pd.DataFrame(occurences)
    #     .transpose()
    #     .rename(columns={i: el for i, el in enumerate(elements_pie_chart)})
    # )
    occurences.fillna(0, inplace=True)
    # Bar chart
    occurences.reset_index(inplace=True)
    occurences.rename(columns={"index": "Key"}, inplace=True)
    if not occurences.empty:
        st.write(
            occurences.pivot(index="Key", columns="Comparison", values="Amount").fillna(
                3
            )
        )
        bar_chart = px.bar(
            occurences, x="Key", y="Amount", color="Comparison", barmode="group"
        )
        bar_chart.update_layout(
            autosize=False, width=_MAX_WIDTH, margin=dict(l=20, r=20, t=20, b=20),
        )
        st.plotly_chart(
            bar_chart,
            config={
                "scrollZoom": False,
                "displayModeBar": False,
                "editable": False,
                "showLink": False,
                "displaylogo": False,
            },
        )

    # Sunburst chart
    if not occurences.empty:
        sunburst_chart = px.sunburst(
            occurences, path=["Comparison", "Key"], values="Amount"
        )
        sunburst_chart.update_layout(
            autosize=False, width=_MAX_WIDTH, margin=dict(l=20, r=20, t=20, b=20),
        )
        st.plotly_chart(
            sunburst_chart,
            config={
                "scrollZoom": False,
                "displayModeBar": False,
                "editable": False,
                "showLink": False,
                "displaylogo": False,
            },
        )

    # Pie chart
    if not occurences.empty:
        selection = st.selectbox(
            "Select element to see in pie chart", options=elements_pie_chart
        )
        pie_chart = px.pie(
            occurences[occurences.Comparison == selection], values="Amount", names="Key"
        )
        pie_chart.update_layout(
            autosize=False, width=_MAX_WIDTH, margin=dict(l=20, r=20, t=20, b=20),
        )
        st.plotly_chart(
            pie_chart,
            config={
                "scrollZoom": False,
                "displayModeBar": False,
                "editable": False,
                "showLink": False,
                "displaylogo": False,
            },
        )


#############################
#
# FILTERING FUNCTIONS
#
#############################


def range_filter(meltedDatset, key=0):
    # Slider and date inputs for day range selection
    min_year = np.amin(meltedDatset.ANNO.values)
    max_year = np.amax(meltedDatset.ANNO.values)

    start = datetime(day=1, month=1, year=min_year)
    end = datetime(day=31, month=12, year=max_year)

    start = st.date_input("Start date", value=start, key=key)
    end = st.date_input("End date", value=end, key=key)

    st.write("You choose ", start, " and ", end)

    start = pd.Timestamp(year=start.year, month=start.month, day=start.day)
    end = pd.Timestamp(year=end.year, month=end.month, day=end.day)

    filteredDatset = meltedDatset[
        (meltedDatset.DATA >= start) & (meltedDatset.DATA <= end)
    ]
    return filteredDatset


def day_filter(meltedDatset, key=0):
    days = st.multiselect(
        "Select day/s",
        options=[
            "Lunedì",
            "Martedì",
            "Mercoledì",
            "Giovedì",
            "Venerdì",
            "Sabato",
            "Domenica",
        ],
        key=key,
    )
    filteredDatset = meltedDatset[meltedDatset.NOME_GIORNO.isin(days)]
    return filteredDatset


def month_filter(meltedDatset, key=0):
    months = st.multiselect(
        "Select day/s",
        options=[
            "Gennaio",
            "Febbraio",
            "Marzo",
            "Aprile",
            "Maggio",
            "Giugno",
            "Luglio",
            "Agosto",
            "Settembre",
            "Ottobre",
            "Novembre",
            "Dicembre",
        ],
        key=key,
    )
    filteredDatset = meltedDatset[meltedDatset.MESE.isin(months)]
    return filteredDatset


#############################
#
# SUMMARY FUNCTIONS
#
#############################


def range_summary(meltedDatset, type_analysis, key=0):
    filteredDatset = range_filter(meltedDatset)
    if type_analysis == "Normal":
        draw_charts_summary(filteredDatset)
    else:
        draw_charts_summary_trend(filteredDatset)


def day_summary(meltedDatset, key=0):
    filteredDatset = day_filter(meltedDatset)
    draw_charts_summary(filteredDatset)


def month_summary(meltedDatset, key=0):
    filteredDatset = month_filter(meltedDatset)
    draw_charts_summary(filteredDatset)


#############################
#
# COMPARISON FUNCTIONS
#
#############################


def range_comparison(meltedDatset):
    number_ranges = st.number_input(
        "Select how many ranges to compare", min_value=0, max_value=3, value=0
    )
    filteredDatset = pd.DataFrame()
    for i in range(number_ranges):
        df = range_filter(meltedDatset, i)
        df["id_comparison"] = i
        filteredDatset = pd.concat([filteredDatset, df])
    if number_ranges > 0:
        draw_charts_comparison(filteredDatset, "range")


def day_comparison(meltedDatset):
    number_days = st.number_input(
        "Select how many days to compare", min_value=0, max_value=3, value=0
    )
    filteredDatset = pd.DataFrame()
    for i in range(number_days):
        df = day_filter(meltedDatset, i)
        df["id_comparison"] = i
        filteredDatset = pd.concat([filteredDatset, df])

    if number_days > 0:
        draw_charts_comparison(filteredDatset, "day")


def month_comparison(meltedDatset):
    number_months = st.number_input(
        "Select how many months to compare", min_value=0, max_value=3, value=0
    )
    filteredDatset = pd.DataFrame()
    for i in range(number_months):
        df = month_filter(meltedDatset, i)
        df["id_comparison"] = i
        filteredDatset = pd.concat([filteredDatset, df])

    if number_months > 0:
        draw_charts_comparison(filteredDatset, "month")


#############################
#
# PREDICTIONS FUNCTIONS
#
#############################


def time_slots_percentage(meltedDatset, type_prediction):
    if type_prediction == "Range":
        filteredDatset = range_filter(meltedDatset)
    elif type_prediction == "Day":
        filteredDatset = day_filter(meltedDatset)
    elif type_prediction == "Month":
        filteredDatset = month_filter(meltedDatset)

    if not filteredDatset.empty:
        filteredDatset["value"] = filteredDatset["value"].astype(str)
        filteredDatset = filteredDatset[filteredDatset.value != "nan"]
        filteredDatset["variable"] = filteredDatset["variable"].astype(int)
        st.write(filteredDatset)
        filteredDatset = (
            filteredDatset[["DATA", "value", "variable"]]
            .groupby(["variable", "value"])
            .agg(["count"])
        )
        # meltedDatset.reset_index(inplace=True)
        filteredDatset.columns = filteredDatset.columns.droplevel(0)
        filteredDatset["percentage"] = (
            filteredDatset.groupby(["variable", "value"])
            .agg({"count": "sum"})
            .groupby(level=0)
            .apply(lambda x: round(100 * x / float(x.sum()), 2))
        )
        st.write(filteredDatset)
        filteredDatset.reset_index(inplace=True)
        filteredDatset = (
            filteredDatset.sort_values("count", ascending=False)
            .groupby("variable", as_index=False)
            .first()
        )
        filteredDatset["variable"] = filteredDatset["variable"].apply(
            lambda x: _NUM_TO_TIME_SLOT[x]
        )
        st.write(filteredDatset)
        dataset = filteredDatset.transpose()
        st.write(dataset)


#############################
#
# MAIN
#
#############################
_max_width_(_MAX_WIDTH)

dataset_2020 = pd.read_csv(Path("./data_2020.csv"))
dataset = pd.read_csv(Path("./data.csv"))
dataset = pd.concat([dataset, dataset_2020], sort=True)
meltedDatset = pd.melt(
    dataset,
    id_vars=["ANNO", "MESE", "SETTIMANE", "GIORNI", "NOME_GIORNO", "ANNO_GIORNO"],
    value_vars=[str(i) for i in range(48)],
)
meltedDatset["DATA"] = (
    meltedDatset.ANNO.astype(str) + "-" + meltedDatset.ANNO_GIORNO.astype(str)
)
meltedDatset["DATA"] = meltedDatset.DATA.apply(lambda x: datetime.strptime(x, "%Y-%j"))


# Set title
st.title("Time")
type_selection = st.sidebar.selectbox(
    "Visualization Type", ("Home", "Summary", "Comparison", "Prediction")
)

if type_selection == "Summary":
    type_summary = st.sidebar.selectbox("Summary Type", ("Range", "Day", "Month"))
    type_analysis = st.sidebar.selectbox("Analysis Type", ("Normal", "Trend"))

    if type_summary == "Range":
        range_summary(meltedDatset, type_analysis)
    elif type_summary == "Day":
        if type_analysis == "Normal":
            day_summary(meltedDatset)
        else:
            pass
    elif type_summary == "Month":
        if type_analysis == "Normal":
            month_summary(meltedDatset)
        else:
            pass
elif type_selection == "Comparison":
    type_comparison = st.sidebar.selectbox("Comparison Type", ("Range", "Day", "Month"))

    if type_comparison == "Range":
        range_comparison(meltedDatset)
    elif type_comparison == "Day":
        day_comparison(meltedDatset)
    elif type_comparison == "Month":
        month_comparison(meltedDatset)
elif type_selection == "Prediction":
    type_prediction = st.sidebar.selectbox("Prediction Type", ("Range", "Day", "Month"))

    time_slots_percentage(meltedDatset, type_prediction)
elif type_selection == "Home":
    with open("./intro.md") as intro:
        mdText = intro.read()
        st.markdown(mdText)
    st.write(dataset)
