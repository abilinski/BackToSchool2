#!/usr/bin/python3

## Scrapes data from https://healthscreening.schools.nyc/surveytesting

import requests
import json
import logging

logging.basicConfig(level=logging.INFO, format="%(levelname)-1.1s %(asctime)s %(module)-10.10s %(message)s")

PAGE_SIZE = 50

def one_call(page):
    """
    Makes one call to get chunked data
    """
    data = {
        "sort" : "",
        "page" : page,
        "pageSize" : PAGE_SIZE,
        "filter" : "",
        "SelectedLocation" : "",
        "SelectedBorough" : ""
    }
    return requests.post("https://healthscreening.schools.nyc/surveytesting/get", data=data).json()

def scrape_all():
    """
    Gets all data, calling for chunks at a time
    """
    ret = []
    i = 1
    res = one_call(i)
    total = res["Total"]
    while (i-1) * PAGE_SIZE < total:
        logging.info("%d of %d", i, total / PAGE_SIZE)
        res = one_call(i)
        ret.extend(res["Data"])
        i+=1
    return ret



if __name__ == "__main__":

    res = scrape_all()
    print(json.dumps(res))
