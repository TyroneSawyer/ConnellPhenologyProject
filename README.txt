Ok, so this is the Connell Phenology Project (title pending revision)
Author: Tyrone Sawyer (they/he), 2023, tsawy6@gmail.com. Contact me any time, I'd love to chat about this!

This is a project I've completed associated with my master's at UQ. It's an attempt to analyse a simply beautiful dataset about tree reproductive patterns created by Bill McDonald over the last 25 years. It's also an introduction to GAMs, which are criminally understudied, they're really really cool and super flexible. It's also an introduction to Autoregressive GAMs, which are... potentially really cool! But as a consequence of trying to pursue them I flew too close to the sun and made a critical error. So hopefully I'll get around to fixing it, but you should know before you dive too too deep. The workflow is still pretty sound, and I don't think my code is tooooo awful? If you're looking for a detailed explanation for what's going on, what the tools are and why, see the report! If you want a detailed explanation for how, I highly recommend Gavin Simpson's body of work on GAMs. The man has answered 3/4s of the Stack exchange questions on the topic personally.

Anyways, you mostly just want a file list:

Anyways, contents I've just uploaded:
        data    -folder containing all the data documents I've been sent by Luke. Was almost all collected by Bill McDonald, with some by Peter O'Reilly. Not all of it is used, but I didn't really wanna tamper with it
        outputs -stores the outputs of the scripts, cleaned data, analysed data, and a folder of all the graphs
        scripts -three of them. data_cleaning first, then statistical_modelling (this one requires either about 24 hours                   or about 5 on a supercomputer) and plot_creation. The titles are fairly self explanatory. Move from raw data->tidy-> some plots. plot creation /does/ need to be run in an environment where do_plots is set to TRUE if you want to get any plots out of it. Sorry, could certainly do to rework that.
        report  -a little paper I wrote about this for my Master's. It's a little feverish, but has some reasonable analysis

Hopefully the code is at least a little self explanatory, it's all commented.

Some variable description:
cleaned_data:
	Tag - unique code for each individual tree
	Namecode - first 4 digits of genus then species. See key at end for all translations
	year - year of data collection
	julian_month - month number of the year
	decimal_date - date with fraction of a year. Convenient ordering variable
buds/flowers/(green/ripe)_fruits - Basically Bill McDonald's arbitrary estimation of the intensity of a crop of the reproductive structure on a scale of 0 to 5. It's not precise, but we're hoping it's properly ordered
	temperatures - average temp in °C of the plot over the month, as estimated by the ANU climate 2.0 project, which estimates (using thin plate splinse also, it turns out!) the temperature at a really fine pretty grid over australia
	vpds - vapour pressure deficits, collected by same process as the above. I think I might have forgotten to mention this in the report, but vpd is supposed to be a good measure of dryness for trees. accounts quite well for how much water they have avaiable at a time. (I imagine especially on a mountain without a ton of groundwater)
	fruits - just the max of green and ripe fruits. they were both just a little... flimsy. One of the problems with the data set is that Bill could just miss a plant going through a whole stage of it's cycle by it going from e.g. bud to green_fruit in a month, skipping flowers. Made it kinda tricky to effectively explore the space. The green/ripe fruits split just expands on that. Plus I'm not 100% sure it was always stuck to. One like, nice sanity check of that first point is that the number of fruit measures is about equal to the number of flowers and the number of buds.


heres's a key for the namecodes. Might be nicer looking with a cbind():
 "ARGYACAC" ,"DORYSASS" ,"BALOINOP", "DIPLAUST" ,"SYZYCREB", "QUINVERD" ,"SARCSTIP", "ACROPUBE", "ATRABENT",
 "DIOSPENT" ,"WILKHUEG", "MISCAUST", "ELATNERV", "ACMEINGE" ,"DYSOFRAS" ,"EUPOLAUR", "ARGYTRIF", "BRACACER",
 "ORITEXCE" ,"WILKAUST", "GUIOSEMI", "EMMEALPH", "VITELIGN" ,"HALFKEND" ,"HELIGLAB", "LITSRETI" ,"DYSORUFU",
 "ACROSUBE", "ACROOBLO", "BEILELLI", "ANTHNITI", "CITRMOOR" ,"POLYELEG", "PHALCHER" ,"POLYCUNN" ,"GUILMONO",
 "MELIMICR" ,"CUPAFLAG", "AKANBIDW", "KARABENT", "EHREACUM"

Argyrodendron actinophyllum, Doryphora sassafras, Baloghia inophylla, Diplazium australe, Syzigium crebinerve, Quintinia verdonii, Sarcopteryx	stipata, Acronychia pubescens, Atractocarpus benthamianus, Diospyros pentamera, 
Wilkiea	huegeliana, Mischocarpus australis, Elattostachys nervosa, Acmena ingens,Dysoxylum fraserianum, Eupomatia	laurina, Argyrodendron	trifoliolatum, Brachychiton	acerifolius, Orites	excelsa, Wilkea austroqueenslandica, 
Guioa	semiglauca,Emmenosperma	alphitonioides,Vitex lignum-vitae, Halfordia	kendack,Helicia	glabrifolia,
Litsea	reticulata,Dysoxylum rufum,Acronychia	suberosa, Acronychia	oblongifolia,Beilschmiedia	elliptica,Anthocarapa	nitidula,Citronella	moorei,Polyscias	elegans,Phaleria octandra,Polyosma	cunninghamii,Guilfoylia	monostylis,Melicope	micrococca, Cupaniopsis	flagelliformis,Akania	bidwillii,  Karrabina benthamiana, Ehretia	acuminata

I do have a list of improvements I'd like to make to the model if I get the time, and they're as follows:


Future directions:
- there's some missing data. I only have part way through 2022, but there is more by now. I think there should be data for 2021 also, but that's missing about half, and maybe quarantine or something.


- there's a big fundamental problem in how fixed effects are selected for. I discuss it a bit in the methods section, but basically fREML can't be used to compare different models, just within the same one. It's unclear how bad this is. Some people say it's a cardinal sin, wheras Jacolian Rij (who's pretty GOATED) seems to think it's not such a big deal. Anyways, I don't like it.

	 Unfortunately, AR GAMs can /only/ be made with this, at least using bam. But, in exchange for running... many times slower (just a guess from some preliminaries) I can run the non-AR models in ML, getting true answers.

	Resolving the AR GAMs is harder. It should be doable with brms? I'm pretty sure it will be. I'm not 100% sure how it optimises yet, though I think it's a little fundamentally different to a method like fREML. It's something that you get warned a lot is really slow, which is why I steered clear in the first place.


- by writing a createDHARMa() function for ARGAMs we can recover access to DHARMa simulations with confidence that they have meaning, which we currently do /not/ have. This would help a lot with sanity checking models. We're currently flying pretty blind.


- it'd be great to have a little error message for when values in labels aren't known!


- there's a small global scaling issue. The data got switched to binomial, but I set the limits such that for fruits that only ever recieved (e.g.) a 4, the value was scaled around that. I didn't implement this scaling for the global model. Not really a major point of the model, but still.

Acknowledgements:
Sincerest thanks to John Dwyer, Bill MacDonald, Simon Wood, Gavin Simpson, and my late Mother. You've all been wonderful, and without you this would not have gotten finished!

Anyways, that's about all I have for you for now. I hope you enjoy!