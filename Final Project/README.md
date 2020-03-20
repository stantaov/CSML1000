

# Credit Card Fraud Detection


![](https://github.com/stantaov/CSML1000/blob/master/Final Project/shinyapp-final-project.jpg)


Abstract

According to Nilson Report (https://nilsonreport.com) the credit card frauds cost business and card issuers around 28 billion dollars in 2018. As we are moving into a cashless society this number is going to grow steadily. When talking about fraudulent transactions, most people think about the values defrauded. However, the total cost of the frauds has to be understood in its whole extension:

1) There is the loss due to the fraud itself (what most people see)
2) There is the cost associated with managing the losses – cancelling orders and refunding charges (exceeds
the cost of the frauds 300%)
3) There is the cost of mistakenly rejecting orders
4) There is the cost of developing and applying mechanisms to avoid fraud.

Therefore, the problem is not only what you lose due the frauds themselves, but the cost to mitigate the negative impact and manage the process (update security measures, block and reissue cards, reimburse customers etc), the cost of building and maintaining mechanisms to prevent the frauds and of course the cost of losing money when you unduly block a sales assuming wrongly it is a fraud.

Therefore, many financial organizations are facing the challenge of building a successful fraud detection model which is easy to maintain and highly effective in spotting frauds and at the same time doesn ́t have a too high false-positive rate. Certain ML algorithms seem to be well suited to address all these issues. They can help automatize the process of adjusting for identifying new types of fraud (the big problem with the current processes) and it can archive an effective identification rate without too many false-negative.

Problem statement /Business problem

Our objective is to spot possible frauds in credit card operations. This identification will be based on the client ́s profile, the seller ́s profile and the data of the transaction itself. The objective is to flag transactions with the high possibility of fraud.

Today frauds spin around 4% of all sales made with credit cards in Brazil. Our client already has mechanisms in place that detects potential fraudulent transactions, however, these mechanisms have two problems:
1) They deploy semi-static rules that generate a scenario where people in the background have to keep looking for new types of fraud to keep adjusting the current model.
2) The process suffers a paradoxical problem: if it is too stringent it creates problems for the clients blocking legit sales if it is too lose it allows a too high level of fraud. A middle term is difficult to archive – even more so when you have to keep adjusting the rules.

To address these two issues, the idea would be to create a model that would not only identify (or at least flag) the suspect transactions but also would identify changes in the patterns and adapt automatically to new fraud patterns.
In order to do that we managed to get a database merging the three data sources (client, seller and transaction) and the idea is to develop a machine learning model which not only assertive (Assertive meaning identify a high percentage of the actual frauds without blocking too many legit ones) but adaptable.

We were verbally informed that the current mechanism spots around 50% (2% of the total) of potential frauds and flags around 2% of legit ones (false positive). If that is true (We have no way to verify this information), it means that the current process gets it right at around 2% of all transactions and wrong at 2%. In summary, it can stop 50% of all fraudulent transactions and has a false positive rate of 2%.

It is interesting to notice that although frauds correspond to just 4% of all transactions, they answer for 8% of the total value of the transactions. It means each 1% of fraud elimination corresponds to approximately R$ 16.000.000,00 /month (CAD 5.330.000,00).
