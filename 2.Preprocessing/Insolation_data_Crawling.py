# Using Selenium and Python, get insolation data from 01.01.2010 until 31.12.2018
import pytest
import time
import json
import csv
import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys

# Upload the coordinates of Seoul district centres
f = open('서울_구_중심.csv', 'r', encoding='utf-8')
rdr = csv.reader(f)
a_list=[]
for line in rdr:
    a_list.append(line)
f.close()

class TestAscii():
  def __init__(self, *args, **kwargs):
    self.ascii_list=[]
  
  # Uplood the chromedriver for selenium 
  def setup_method(self):
    self.driver = webdriver.Chrome("/mnt/c/work/chromedriver.exe")
    self.vars = {}
  
  def teardown_method(self):
    self.driver.quit();
  
  def wait_for_window(self, timeout = 2):
    time.sleep(round(timeout / 1000))
    wh_now = self.driver.window_handles
    wh_then = self.vars["window_handles"]
    if len(wh_now) > len(wh_then):
      return set(wh_now).difference(set(wh_then)).pop()
  
  def test_ascii(self):
    # Get the NASA nasa radiation map
    self.driver.get("https://power.larc.nasa.gov/data-access-viewer/")
    self.driver.set_window_size(808, 860)

    # Waiting until load the site
    time.sleep(30)

    self.driver.find_element(By.CSS_SELECTOR, ".enable-btn").click()
    element = self.driver.find_element(By.ID, "dijit__WidgetBase_4")
    actions = ActionChains(self.driver)
    actions.move_to_element(element).click_and_hold().perform()
    element = self.driver.find_element(By.ID, "dijit__WidgetBase_4")
    actions = ActionChains(self.driver)
    actions.move_to_element(element).perform()
    element = self.driver.find_element(By.ID, "dijit__WidgetBase_4")
    actions = ActionChains(self.driver)
    actions.move_to_element(element).release().perform()

    # Selct the start date
    self.driver.find_element(By.ID, "datepickerstart").clear()
    self.driver.find_element(By.ID, "datepickerstart").click()
    self.driver.find_element(By.ID, "datepickerstart").send_keys("01/01/2010")
    self.driver.find_element(By.ID, "userinput").click()

    # Select the end date
    self.driver.find_element(By.ID, "datepickerend").clear()
    self.driver.find_element(By.ID, "datepickerend").click()
    self.driver.find_element(By.ID, "datepickerend").send_keys("12/31/2018")
    self.driver.find_element(By.ID, "Sizing and Pointing of Solar Panels and for Solar Thermal Applications_anchor").click()
    self.driver.find_element(By.ID, "Solar Cooking_anchor").click()
    self.driver.find_element(By.ID, "Tilted Solar Panels_anchor").click()
    
    # Start from 1 and add 1 when it comes to run one time
    count = 1
    for a in a_list[1:]:
      print(a[0],a[1])
      # Every 500 times, save the collected insolation data as csv file 
      if count%500==0:
        dataframe = pd.DataFrame(self.ascii_list)
        dataframe.to_csv("ascii_csv_solar_panel"+str(count)+".csv",header=False,index=False)
        self.ascii_list = []
    
      # Select the latdaily of Seoul district centres
      self.driver.find_element(By.ID, "latdaily").click()
      self.driver.find_element(By.ID, "latdaily").send_keys(a[1])

      # Select the londaily of Seoul district centres
      self.driver.find_element(By.ID, "londaily").click()
      self.driver.find_element(By.ID, "londaily").send_keys(a[0])
      
      # Click the button Summit
      self.driver.find_element(By.ID, "testbuttondaily").click()
      self.vars["window_handles"] = self.driver.window_handles

      
      wait = WebDriverWait(self.driver, 1800)
      element = wait.until(EC.visibility_of_element_located((By.CSS_SELECTOR, "#exportASCII > font")))
      print(element.text)
      
      # Click the button ASCII
      # When Crawling is stopped from any reasons(slow loading), do not stop and keep running
      try:
        self.driver.find_element(By.CSS_SELECTOR, "#exportASCII > font").click()
      except Exception:
        print("error to click ascii")
        continue
      
      self.vars["win8630"] = self.wait_for_window(2000)
      self.vars["root"] = self.driver.current_window_handle
      time.sleep(10)
      
      # Go to next page and get the ascii data of insolation data
      self.driver.switch_to.window(self.vars["win8630"])
      pre = self.driver.find_element(By.TAG_NAME,"pre")

      # Split the uesless part and start from the line 14 from the top
      for a in pre.text.split('\n')[14:]:
        self.ascii_list.append(a)
      print(len(self.ascii_list))
      print("count=",count)
      self.driver.close()

      # Go back to first page
      self.driver.switch_to.window(self.vars["root"])
      self.driver.find_element(By.ID, "ordermore").click()
      element = self.driver.find_element(By.ID, "cleargraphicdaily")
      actions = ActionChains(self.driver)
      actions.move_to_element(element).click_and_hold().perform()
      element = self.driver.find_element(By.ID, "cleargraphicdaily")
      actions = ActionChains(self.driver)
      actions.move_to_element(element).perform()
      element = self.driver.find_element(By.ID, "cleargraphicdaily")
      actions = ActionChains(self.driver)
      actions.move_to_element(element).release().perform()
      self.driver.find_element(By.ID, "cleargraphicdaily").click()

      count+=1

    return self.ascii_list

# Run Crawling
ts = TestAscii()
ts.setup_method()
asc_list = ts.test_ascii()
dataframe = pd.DataFrame(asc_list)
print(dataframe)

# Save the whole data at the last
dataframe.to_csv("ascii_csv_solar_panel.csv",header=False,index=False)