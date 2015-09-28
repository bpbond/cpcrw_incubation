Notes
===========

19 August 2015
----------------------
From Peyton:
>So I just wanted to send you guys some general moisture and density estimations from the CPCRW Active Layer (AL) soils for Ben’s “Temperature and Moisture Controls” project. Basically, I calculated moisture on both a gravimetric and volumetric basis. I did it using both whole core and subsample measurements. I oven dried the subsamples, whereas the whole core soils were air-dried (starting Aug 11th). I was hoping to calculate porosity (using bulk density and particle density measurements) and unfortunately, due to all the rocks in the cores, I could not get accurate particle volumes. Alas, had I chosen to go with a saturation method, I could have gotten this value (as well as some tension values that Vanessa would like). I still have 3 shitty intact cores (*note: not schisty, totally shitty/unreliable cores that I removed from all analyses) that I could saturate and work with for porosity and tension, but I doubt these cores would be representative and we would have to specify that our values were lab-based measurements. But all in all, I will have moisture, bulk density, soil CN, soluble CN, and enzyme activities for the pre-incubation samples.
>
>So, I think that I will go with a volumetric estimation for maintaining these cores at “field moist” values and keep the cores ~ 28% moist. For the drought-induced soils, I was considering 5% moisture. Look over the spreadsheet, let me know your thoughts. Soils are in the environmental chambers and either on saturated pore plates, or not and I can start the incubation anytime

01 September 2015
----------------------
From Peyton:
>The incubation got off to a late start. I ended up bringing all the cores up to (roughly) the same moisture level in each incubation chamber and then had to schedule the measurements for when I had 7 days I could come into the lab to take measurements. (which was difficult due to coordinating work schedules with my husband, vacation, etc) Anyways, it finally got off to its official start yesterday! Yeah. I uploaded the Picarro files and a spreadsheet of ports, weights, Picarro schedule, etc. to the “Ben Picarro Data >> CPCRW >> 31 Aug 2015” folders on the shared drive. I also attached it as a compressed file to this email. Please review if you have a chance to verify that the Picarro is behaving and recording things properly. I have dismantled it and moved it up between chambers quite a bit and would hate to have caused an issue and not known until after the incubation.
>
>Basically, each core gets a 2 minute measurement 3x. I turn it on and set it to measure the ambient core for 30-60 minutes (depending on when the Picarro finally starts to log data and giving time for my CO2 to dissipate) before it start measuring the cores. I measure the 6 cores in each treatment one after the other ( 6 cores x 2 mins = 12 mins) and then put that cycle on repeat for two more times (total of 3x per core = 36 mins per treatment) and then measure Port 10 (ambient) core in between treatments. The next moisture treatment gets the same schedule (using different Picarro caps/ports). However, in the 20C chamber I have to use the same port #’s for 2 moisture treatments (field moist and controlled dry down) just because we don’t have enough ports.
>
>If having triplicate measurements is overkill on the data management and calculations, please let me know!!! Only because it takes more time and I would be happy to have it run for less time. However, I would feel more comfortable with at least duplicate measurements. If you also want to change the order of how I measure the cores (i.e. randomizing the order of cores measured), let me know and I will do my best to change things.
>
>Finally – if as the week progresses you decide that you only need/want the first 5 days of measurement (and not 7 whole days) and then go to twice a week (I can even do three times next week) – please, please let me know. The measuring of these cores is unexpectedly more time consuming than I imagined (based on all the port switching, Picarro set up./take down, moving it, the computer, the pump, etc. up and down the hall every day, etc.) and I don’t mind doing it occasionally – but only if we need to data.

05 September 2015
----------------------
Added diagnostics as part of `README.md`.

19 September 2015
----------------------
Removed incomplete final line in CFADS2283-20150918-192831Z-DataLog_User.dat:
>2015-09-18                20:26:25.127              260.51834638              6252.440313               261.5

22 September 2015
----------------------

From Peyton:
>There was a weird looking measurement today when running the field moist 20C cores – basically there was a long stretch between 2 cores and some spikes (when I looked at the figures the Picarro spits out in real time). So, therefore I just measured them again and because the field moist cores share the same valve numbers as the controlled drought cores (which get measured when I am done measuring the field moist and drought cores), I had to use different valves/ports for the controlled drought cores (i.e. 4, 2, 12, … versus 6, 16, 13, 14,…) . THIS WILL ALL BE NOTED IN THE SPREADSHEET- But I thought I would give you a head’s up regardless.
> 
>I am also planning on doing a test run of core, ambient, core, ambient, core, ambient for a set of cores when I am finished with all the normal measurements. This way, you can see if it changes readings significantly or whether we should implement ambient readings between cores in the future, etc.
 
Added a `3-remove.R` script and processes a `data/removals.csv` file to remove problematic data. Its first and currently only target is samplenum 385--the Picarro was left running from 2015-09-08 21:58:39 to 2015-09-11 16:49:09, just recording ambient.

28 September 2015
----------------------

New data added. Added an entry for `removals` file, for samplenum 659, same reason as before--Picarro just logged ambient for 2-3 days.
