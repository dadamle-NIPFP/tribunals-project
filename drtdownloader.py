import time
import sys
import lxml.html
import json
import calendar
import traceback
from datetime import date, timedelta
from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import ElementNotVisibleException

class Hearing:
    def __init__(self, tableRow, story):
        self.computerID, self.caseNo, self.petitioner, self.appealNo = tableRow[:4]
        self.date, self.orderType, self.tribunal = tableRow[4:7]
        self.story = story

def getDateElement(driver, calendarNumber, inputDate):
    desiredDay = inputDate.day
    desiredMonth = inputDate.month
    desiredYear = inputDate.year
    monthName = calendar.month_name[desiredMonth]
    calendarId = "ContentPlaceHolder1_CalendarExtender" + calendarNumber + "_title"
    dateXpath = ("//div[@class='ajax__calendar_day'"
            " and contains(@title, '" + monthName + " " + str(desiredDay).zfill(2) + ",')"
            " and contains(@id, 'CalendarExtender" + calendarNumber + "')]")
    prevArrowId = "ContentPlaceHolder1_CalendarExtender" + calendarNumber + "_prevArrow"
    while True:
        time.sleep(1)
        monthSheet = getElementById(driver, calendarId)
        # If the calendar is not visible on the screen, we get ElementNotVisible exception
        driver.execute_script("return arguments[0].scrollIntoView();", monthSheet)
        currentMonthYear = monthSheet.text
        monthIsLocated = (monthName in currentMonthYear and str(desiredYear) in currentMonthYear)
        if monthIsLocated: return getElementByXpath(driver, dateXpath)
        else: getElementById(driver, prevArrowId).click()

def getElementById(driver, elementId, maxTime=20):
    xpath = "//*[@id='" + elementId + "']"
    return getElementByXpath(driver, xpath, maxTime)

def getElementByXpath(driver, xpath, maxTime=20):
    timeTaken = 0
    while True:
        try:
            elements = driver.find_elements_by_xpath(xpath)
            number = len(elements)
            err = ("XXXFound " + str(number) + " by xpath. Their id:"
                    "\n".join(el.get_attribute('id') for el in elements))
            assert number == 1, err
            return elements[0]
        except:
            if timeTaken > maxTime:
                print("XXXIn getElementByXpath, couldn't click xpath=" + xpath
                        + " after " + str(timeTaken) + " seconds.")
                raise
            else:
                print("XXXNot found by xpath: " + xpath +
                    ", trying again. Time taken so far is" + str(timeTaken))
            time.sleep(5)
            timeTaken += 5

""" This is the function that does all the work. It takes a range of dates, gets data for each of
them, and writes them to the file you specify. We are currently using it with only one date per call,
but it has the capacity to take any range of dates. """
def getDataForDates(driver, outputFile, startDate, endDate):
    print("Start Date= " + str(startDate))
    print("End Date= " + str(endDate))
    # Select the location
    select = Select(getElementById(driver, "ContentPlaceHolder1_ddlLocation"))
    select.select_by_visible_text("Delhi DRT-3")
    # Radio button. Select the kind of order: We want "daily orders"
    getElementById(driver, "ContentPlaceHolder1_rbtn_0").click()
    # Set the Start Date
    getElementById(driver, "ContentPlaceHolder1_img").click() #click on calendar image
    getDateElement(driver, "1", startDate).click()
    # Set the End Date
    time.sleep(2)
    getElementById(driver, "ContentPlaceHolder1_ImageButton1").click() #click on calendar image
    getDateElement(driver, "2", endDate).click()
    #Now click on "View Order"
    getElementById(driver, "ContentPlaceHolder1_btnViewOrder").click()
    # If there is no data, then warningElement.text tells us so.
    time.sleep(2)
    warningElement = getElementById(driver, "ContentPlaceHolder1_lblmsg")
    if warningElement.text: # This means there is no data.
        outputFile.write("[]")
        return None
    # Get all the content of the table html
    tableEl = getElementById(driver, "ContentPlaceHolder1_GridView1")
    tableSource = tableEl.get_attribute('innerHTML')
    rows = lxml.html.fromstring(tableSource)
    rows = rows[1:] # The first row of the table is the headers.
    numberOfHearings = len(rows)
    print("Number of hearings = " + str(numberOfHearings))
    outputFile.write("[")
    for i, row in enumerate(rows): # Loop through each hearing.
        jsonHearing = getDataForHearing(driver, i, row)
        if i < (numberOfHearings-1): jsonHearing += "," # We need commas except after the last item.
        print("Got data for row index: {}".format(i))
        outputFile.write(jsonHearing)
    outputFile.write("]")
    return None

def getDataForHearing(driver, index, row):
    time.sleep(2)
    values = [el.text.strip() for el in row] # The first row is the header row.
    detailsViewId = "ContentPlaceHolder1_GridView1_lnkView_" + str(index)
    getElementById(driver, detailsViewId).click()
    time.sleep(5) # Now we are in the hearing details view. The content is in the "body" div.
    story = getElementByXpath(driver, "//div[@class='modal-body']").text.strip()
    jsonHearing = json.dumps(Hearing(values, story).__dict__)
    try:
        closeButton = getElementByXpath(driver, "//div[@class='modal-footer']/button")
        driver.execute_script("return arguments[0].scrollIntoView();", closeButton)
    # Common issue: we get elementNotVisibleException on clicking the closeButton.
    # So before we click, we try to scroll to it.
        closeButton.click() # Close the view, and _then_ write to file.
        return jsonHearing
    except ElementNotVisibleException:
        print("Bottom close button failed. Current hearing is: " + jsonHearing + 
                "Trying top close button.")
        try:
            closeButton = getElementByXpath(driver, "//div[@class='modal-header']/button[@type='button']")
            driver.execute_script("return arguments[0].scrollIntoView();", closeButton)
            closeButton.click() # Close the view, and _then_ write to file.
            print("Top close button succeeded.")
            return jsonHearing
        except ElementNotVisibleException:
            print("Top close button also failed. Giving up here, will retry the entire day.")
            raise

def getDates():
    usageMessage = ("Usage: 'python " + sys.argv[0]+ " startDate [endDate]', date format is yyyy-mm-dd")
    if len(sys.argv) not in [2, 3]: exit(usageMessage)
    startDateStrs = [int(i) for i in sys.argv[1].split('-')]
    startDate = date(*startDateStrs)
    if len(sys.argv) == 3:
        endDateStrs = [int(i) for i in sys.argv[2].split('-')]
        endDate = date(*endDateStrs)
        if startDate > endDate: exit("startDate is after endDate")
    else: endDate = startDate # Use the startDate as the endDate
    print("Dates: " + str(startDate) + " to " + str(endDate))
    return (startDate, endDate)

""" This script takes a range of dates. It then gets the daily orders for each date in that range,
and writes all the details into a file. The filename is yyy-mm-dd."""
def main():
    startDate, endDate = getDates()
    oneDay = timedelta(days=1)
    currentDate = endDate
    while currentDate >= startDate: # We iterate starting from the nearest date backwards.
        print("Getting data for date: " + str(currentDate))
        try:
            outputFile = open(currentDate.isoformat(), 'w')
            # Do we need to create a new driver for every date? Probably not, but ok for now.
            driver = webdriver.Firefox()
            driver.implicitly_wait(5); #seconds
            driver.get("http://drt.gov.in/ViewDailyOrderFinalOrderRecoveryCertificate.aspx")
            # getDataForDates can take a range of dates, though we are giving it only one date at a time.
            getDataForDates(driver, outputFile, currentDate, currentDate)
        except KeyboardInterrupt:
            raise
        except: # All interrupts other than keyboardinterrupt: we want to retry.
            print("XXXception! Date is " + str(currentDate) + ". Exception trace is:")
            traceback.print_exc(file=sys.stdout)
            print("XXX, date is " + str(currentDate) + ". Retrying the same date")
            currentDate += oneDay
        finally:
            driver.quit()
            outputFile.close()
            print("Closing driver.")
        currentDate -= oneDay

if __name__ == "__main__":
    main()
