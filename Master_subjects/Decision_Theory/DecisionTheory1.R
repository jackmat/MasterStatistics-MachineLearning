#########LAb1
###Conditional Probability of having the illness and showing the symptons (i.e. symptoms|A)
symptomsA<-0.25
symptomsB<-0.15
symptomsC<-0.05
A<-0.001
B<-0.005
C<-0.01
#one can only have one illness

##Prob(symptoms|no A)= 0.005

symptomsnoillness<-0.005

#Probability 
### Prob(A,symptoms)= Prob(symptoms|A)*Prob(A)
### Prob(A,symptoms)= Prob(A|symptoms)*Prob(symptoms)
probA<-symptomsA*symptomsnoillness

#probability(illness |symptoms) <- prob(symptoms|illness)*prob(illness)/
#             prob(symptoms|illness)*prob(illness)+prob(symptoms|Notillness)*prob(notillness)
### = joint(symtoms, illness)/(joint(symtoms, illness)+ joint(symptoms, not illness))

Agivensymptoms<-symptomsA/(symptomsA+symptomsnoillness) ##98% of the cases
Bgivensymptoms<-symptomsB/(symptomsB+symptomsnoillness) ##96.7%
Cgivensymptoms<-symptomsC/(symptomsC+symptomsnoillness) ##90%


##b
###pROBABILITY OF PEOPLE HAVING THE ILLNESS GIVEN THE SYMPTOMS
symptomsA*A/symptomsA*A+symptomsnoillness*(1-A)



