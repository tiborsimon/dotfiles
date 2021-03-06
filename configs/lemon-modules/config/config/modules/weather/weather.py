import os
import sys
import time
from datetime import datetime

import requests

ICONS = {
    "01d": "",  # clear sky
    "02d": "",  # few clouds
    "03d": "",  # scattered clouds
    "04d": "",  # broken clouds
    "09d": "",  # shower rain
    "10d": "",  # rain
    "11d": "",  # thunderstorm
    "13d": "",  # snow
    "50d": "",  # mist
    "01n": "",  # clear sky
    "02n": "",  # few clouds
    "03n": "",  # scattered clouds
    "04n": "",  # broken clouds
    "09n": "",  # shower rain
    "10n": "",  # rain
    "11n": "",  # thunderstorm
    "13n": "",  # snow
    "50n": "",  # mist
    "sunrise": "",
    "sunset": "",
    "degrees": "°C",
}

ICON_SUNSET = ""
ICON_SUNRISE = ""
ICON_DEGREES = "°C"

API_KEY = os.environ.get("API_KEY_OPEN_WEATHER_MAP")
if not API_KEY:
    print("API_KEY_OPEN_WEATHER_MAP environment variable is missing!")
    sys.exit(-1)

LOCATION = "Budapest,hu"

URL = "https://api.openweathermap.org/data/2.5/weather"
PARAMETERS = "?q={}&appid={}&units=metric"
PARAMETERS = PARAMETERS.format(LOCATION, API_KEY)
URL += PARAMETERS


def get_data():
    response = requests.get(URL)

    if response.status_code == 200:
        data = response.json()
        return data
    return None


def main():
    data = get_data()

    temp = data["main"]["temp"]
    # description = data["weather"][0]["description"]
    sunset = data["sys"]["sunset"]
    sunrise = data["sys"]["sunrise"]

    temp = round(temp, 1)
    sunset = datetime.fromtimestamp(sunset)
    sunrise = datetime.fromtimestamp(sunrise)

    temp = f"{temp}{ICON_DEGREES}"

    now = datetime.fromtimestamp(time.time())

    if now < sunrise:
        sunrise = sunrise.strftime("%H:%M")
        sun = f"%{{T5}}{ICON_SUNRISE} %{{T1}}{sunrise}"
    else:
        sunset = sunset.strftime("%H:%M")
        sun = f"%{{T5}}{ICON_SUNSET} %{{T1}}{sunset}"

        print(f"%{{T1}}%{{A:weather:}}{temp}  {sun}%{{A}}")


if __name__ == "__main__":
    main()
