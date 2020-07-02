# Predicting Mushroom Edibility
In 1981, The Audubon Society Field Guide to North American Mushrooms created a data set consisting of 23 different types of mushrooms. These mushrooms were categorized into two groups based on their edibility: edible or poisonous. Mushrooms that were not verifiably edible or not safe to eat were considered to be poisonous. In this project, we predicted whether a mushroom is safe to eat.

A brief exploration of the data was conducted, which found that odor was a good indication of a mushroom's edibility. Depending on the odor, the mushroom can be safe to eat, according to the data. For example, mushrooms with a foul, fishy, or spicy odor were not safe to eat. Meanwhile, mushrooms that smelled like almonds or anise were considered safe to eat. The only area of uncertainty here was when the mushrooms didn't have an odor.

Another feature that is useful for determining edibility is the gill color. Mushrooms with green and buff gill colors were poisonous while those with orange and red gill colors were edible. Similarly, the stalk color above the mushroom's ring (annulus) can help determine edibility as well, with buff, cinnamon, and yellow being unsafe to eat whereas gray, orange, and red were safe.

After splitting the data into a training set (80%) and a test set (20%), we used three models to predict mushroom edibility: logistic regeression, k-nearest neighbors, and a classification tree. We achieved an **accuracy of 100%** for each model! However, there were warnings presented when fitting the logistic regression model.

The data set can be accessed here: 
<https://www.kaggle.com/uciml/mushroom-classification>
