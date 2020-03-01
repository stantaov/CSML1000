
# Unsupervised Clustering for Classifying Clients


![](https://github.com/stantaov/CSML1000/blob/master/Assignment%202/shinyapp.jpg)


Based on the client profile identify the chances of success in reaching the client and once reaching it the chances of selling to it. Based on this percentage we would be able to classify the potential clients by “chance to be reached” and “chance to sell”. These two percentages would allow us to group these clients. This grouping would allow better management of the sales effort.
In summary, the objective is to identify patterns associating with the profile of the client with success in reaching and selling.
The product being sold is a credit card and the client base is the pre-existing list of people having bank accounts in a large bank in Brazil.
The bank doesn ́t operate the sales itself. A third part company is sub-contracted to do the actual sales campaign through the phone. The database with the client’s details is forwarded to this company by the bank.
From the perspective of this third part company, the objective is how to maximize sales with the minimum sales effort.
To address this issue the classification of the clients in terms of their propensity to be reached and to buy the product is crucial.
In addition to that, the data exploration also gives insights about possible strategies of pre- processing the database which could eliminate the need for some types of calls.
Another important point linked with the process is the fact that our concern here is how to increase the sales effectiveness, we assume that every client in the list, theoretically can have the credit card. There is no negative-classification in the sense that some profiles cannot get the card. The idea is just to identify if a profile is likely/unlikely to purchase a card. A priori eligibility for having the card was done by the bank when preparing the mailing sent to the third part company. Therefore we are not faced here with eventual ethical dilemmas (See responsible AI in consumer enterprise).
