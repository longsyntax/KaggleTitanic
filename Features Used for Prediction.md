# KaggleTitanic

www.kaggle.com/longsyntax

Latest submission of the Kaggle competition to predict Titanic survivors. 

Code is in R, using the caret library. 

# Features Used/Improvements Made based on the training set
Replaced possibly erronous values where Fare = 0 with median values of the Fares for the corresponding Pclass.

Used globally search regular expression and print logical [grepl()] to extract Title from the Name.

Used Linear Model value fitting with Titles, Fare, Siblings, Sex as features to predict missing age values.

Replaced a negative age value with its modulo.

Created a feature denoting if a passenger is either Female or Young(<=21 years old).

Used SibSp and Parch to calculate Family Size and grouped them in the best posible way based on Survival probability.

Used caret's train() to create a model using Random Forest algorithm with "Pclass" + "Sex" + "YoungOrFemale" + "FamilySize" + "Title" as features.
