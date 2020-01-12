# music-recommanded-system

Recommendation engines are vital to many of today's most successful firms. The dataset contains three .csv files, obtained from the Million Song Dataset Project, of derived user ratings of songs based on each user's listening behavior. The goal of this project is to recommend specific songs to specific users. 

Step 1: split training set and let it contains 84% of the observations. Validation set A to be used for tuning the collaborative filtering model, with 4% of the observations.Validation set B to be used for blending, with 4% of the observations. Testing set with 8% of the observations. 

Step 2: use the Incomplete function in the softImpute package to construct an incomplete training set matrix. 

Step 3: Let X denote the "complete" rating matrix, denotes either the observed rating if user i actually rated song j. 

Step 4: use the function biScale in the softImpute package to fit the model  using least square approach, look for what are the three most popular songs after removing for the bias due to user's affinity for rating songs highly or lowly. 

Step 5: show the out-of- sample performance of the fitted model on the previously constructed test set, including the values of test set MAE, RMSE, and the OSR^2 values. 

### collaborative filtering model
