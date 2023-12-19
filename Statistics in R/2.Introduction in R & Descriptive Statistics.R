######################### Παραδειγμα 1ο: #####################################################################

#Εισαγωγή Δεδομένων

require(MASS)

infert


#Περιγραφή/Ορισμός αντικείμενων μεταβλητών

class(infert) #Ποια είναι η κλάση του infert;

#Μια πρώτη εικόνα των δεδομένων

head(infert)

#Ποιες είναι οι μεταβλητές του συνόλου δεδομένων infert;

names(infert)

#Χαρακτηρίστε τις μεταβλητές ως προς το είδος τους.
str(infert)

#Διόρθωση μεταβλητών

#education, age, case, induced(abortions), spontaneous(abortions)

infert$education<-factor(infert$education, order=T)
is.ordered(infert$education)

infert$case<-factor(infert$case)

is.ordered(infert$case)

infert$induced<-factor(infert$induced, order=T)

is.ordered(infert$induced)

infert$spontaneous<-factor(infert$spontaneous, order = TRUE)

is.ordered(infert$spontaneous)

#Δες τις αλλαγές
str(infert)


#Επισκόπιση με τις σωστές μεταβλητές
summary(infert)



#Περιγραφική Στατιστική

#Πίνακες Συχνοτήτων

####Education


freq.edu<-table(infert$education)


freqedu<-as.data.frame(freq.edu)


colnames(freqedu)<-c("Education","Frequency")


levels(freqedu$Education)<-c("0-5 years","6-11 years",">=12 years")


freqedu

## Barplot

bar.freq.edu<-barplot(freq.edu,main="Education Distribution", 
                      
                      xlab="Education years",ylab="Frequency",
                      
                      horiz=TRUE ,cex.names=0.8)

## Pie chart

pie.freq.edu<-pie(freq.edu,
                  
                  col=rainbow(length(freq.edu)),
                  
                  main="Education years")

legend("topright",
       
       names(freq.edu),
       
       fill=rainbow(length(freq.edu)))


####Induced

freq.induced<-table(infert$induced)
freqinduced<-as.data.frame(freq.induced)

colnames(freqinduced)<-c("Induced","Frequency")
levels(freqinduced$Induced)<-c("0","1","2 or more")

freqinduced

## Barplot

bar.freq.edu<-barplot(freq.induced,main="Induced Distribution", 
                      
                      xlab="Induced", ylab="Frequency",
                      
                      horiz=TRUE ,cex.names=0.8,
                      
                      col=rainbow(length(freq.induced)))

## Pie chart

pie.freq.edu<-pie(freq.induced, main="Induced", 
                  
                  col=rainbow(length(freq.induced)) )



legend("topright", names(freq.induced),
       
       fill=rainbow(length(freq.induced)))


####Spontaneous

freq.spontaneous<-table(infert$spontaneous)
freq.spontaneous
freqspontaneous<-as.data.frame(freq.spontaneous)
freq.spontaneous
colnames(freqspontaneous)<-c('Spontaneous','Frequency')
levels(freqspontaneous$Spontaneous)<-c('0','1','2 or more')

freqspontaneous

## Barplot
spont_bar<-barplot(freq.spontaneous,
                   main = 'Spontaneous Distribution',
                   xlab = 'Frequency',
                   horiz = T,
                   col = rainbow(length(levels(infert$spontaneous))),
                   cex.names = 0.8)



## Pie chart
spont_pie<-pie(freq.spontaneous,
               main = 'Spontaneous Distribution',
               col = rainbow(length(levels(infert$spontaneous))))


legend("topright", names(freq.spontaneous),
       
       fill=rainbow(length(freq.spontaneous)))


#Πίνακες Σχετικών Συχνοτήτων

####Education

relfreq.edu<-prop.table(table(infert$education))


relfreqedu<-as.data.frame(relfreq.edu)

colnames(relfreqedu)<-c("Education","Relative Frequency")


levels(relfreqedu$Education)<-c("0-5years","6-11 years",">=12years")

tab<-cbind(freqedu,relfreqedu[,2])


colnames(tab)<-c("Education","Frequency","Relative Frequency")

tab

####Induced

relfreq.induced<-prop.table(table(infert$induced))
relfreqinduced<-as.data.frame(relfreq.induced)

colnames(relfreqinduced)<-c("Induced","Relative Frequency")
levels(relfreqinduced$Induced)<-c("0","1","2 or more")

tab2<-cbind(freqinduced,relfreqinduced[,2])
colnames(tab2)<-c("Induced","Frequency","Relative Frequency")
tab2

####Spontaneous
relfreq.spontaneous<-prop.table(table(infert$spontaneous))
relfreqspontaneous<-as.data.frame(relfreq.spontaneous)

colnames(relfreqspontaneous)<-c("Spontaneous","Relative Frequency")
levels(relfreqspontaneous$Spontaneous)<-c("0","1","2 or more")

tab3<-cbind(freqspontaneous,relfreqspontaneous[,2])
colnames(tab3)<-c("Spontaneous","Frequency","Relative Frequency")
tab3

############# Για εκτύπωση και μόνο #########################
require(gridExtra)

png("tab.png")
p<-tableGrob(tab)
grid.arrange(p)
dev.off()

png("tab2.png")
p2<-tableGrob(tab2)
grid.arrange(p2)
dev.off()

png("tab3.png")
p3<-tableGrob(tab3)
grid.arrange(p3)
dev.off()
#############################################################

attach(infert)

#Να κατασκευάσετε πίνακα συχνοτήτων, σχετικών συχνοτήτων 
#για την μεταβλητή education 
#για τους υγιείς [case==0] και τους ασθενείς [case==1] ξεχωριστά

#Πίνακες Συχνοτήτων

freqedu0<-table(education[case==0])



freqedu1<-table(education[case==1])


freqedu0<-as.data.frame(freqedu0)
freqedu1<-as.data.frame(freqedu1)



tablefreqcase<-cbind(freqedu0,freqedu1[,2])
colnames(tablefreqcase)<-c("Education","Case=0","Case=1")
levels(tablefreqcase$Education)<-c("0-5 years",
                                   "6-11 years",">=12 years")


tablefreqcase

#Πίνακες Σχετικών Συχνοτήτων

# table(education[case==0])/length(education[case==0])

# table(education[case==1])/length(education[case==1])


relfreqedu0<-prop.table(table(education[case==0]))



relfreqedu1<-prop.table(table(education[case==1]))



relfreqedu0<-as.data.frame(relfreqedu0)
relfreqedu1<-as.data.frame(relfreqedu1)


tablerelfreqcase<-cbind(relfreqedu0,relfreqedu1[,2])
colnames(tablerelfreqcase)<-c("Education","Case=0","Case=1")


levels(tablerelfreqcase$Education)<-c("0-5 years",
                                      "6-11 years",">=12 years")


tablerelfreqcase

############# Για εκτύπωση και μόνο ##################

png("tableFreqCase.png")
qqq<-tableGrob(tablefreqcase)
grid.arrange(qqq)
dev.off()

png("tableRelFreqCase.png")
aw<-tableGrob(tablerelfreqcase)
grid.arrange(aw)
dev.off()
########################################


#Να κατασκευάσετε Crosstabulation Matrix όπου οι στήλες θα είναι η induced 
#και οι γραμμές η spontaneous μεταβλητή, 

#dok<-xtabs(~ spontaneous+induced,infert)

dok<-table(spontaneous,induced)

colnames(dok)<-c("0","1","2 or more")
rownames(dok)<-c("0","1","2 or more")

dok
############# Για εκτύπωση και μόνο ##################
png("Crosstabulation Matrix.png")
dokqqq<-tableGrob(dok)
grid.arrange(dokqqq)
dev.off()
############# εκτύπωση τέλος ##################

#στην συνέχεια με το chi-squared contingency table test να εξετάσετε 
#αν οι δυο μεταβλητές είναι εξαρτημένες ή όχι.

chisq.test(spontaneous,induced)

#Να γράψετε την μηδενική και την εναλλακτική υπόθεση καθως 
#και το συμπέρασμα του παραπάνω ελέγχου.


#Υπολογίστε για την μεταβλητή age για κάθε δείγμα (Control- Patient) τις εξής παραμέτρους: 
#μέση τιμή, διάμεσο, τυπική απόκλιση, διασπορά, λοξότητα, κύρτωση, εύρος και ποσοστημόρια. 


mean0<-mean(age[case==0])

mean1<-mean(age[case==1])

median0<-median(age[case==0])

median1<-median(age[case==1])

sd0<-sd(age[case==0])
sd1<-sd(age[case==1])
var0<-var(age[case==0])
var1<-var(age[case==1])

x <- c(0.0,0.1,0.25,0.50,0.75,1)

library(e1071)

skew0<-skewness(age[case==0])
skew1<-skewness(age[case==1])

kurt0<-kurtosis(age[case==0])
kurt1<-kurtosis(age[case==1])

rangecase0<-range(age[case==0])
rangecase0<-diff(rangecase0)
rangecase0

rangecase1<-range(age[case==1])
rangecase1<-diff(rangecase1)
rangecase1

x <- c(0.0,0,10,0.25,0.50,0.75,1)

quantile0<-quantile(age[case==0])
quantile0<-as.data.frame(quantile0)

quantile1<-quantile(age[case==1])
quantile1<-as.data.frame(quantile1)


results0<-rbind(mean0,median0,sd0,var0,skew0,kurt0,rangecase0,quantile0)
results0<-as.data.frame(results0)

results1<-rbind(mean1,median1,sd1,var1,skew1,kurt1,rangecase1,quantile1)
results1<-as.data.frame(results1)

results<-cbind(results0,results1[,1])
colnames(results)<-c("case=0","case=1")
rownames(results)<-c("Mean","Median","SD","Var","Skewness","Kurtosis","Range",
                     "Quantile 0%","Quantile 25%","Quantile 50%", "Quantile 75%","Quantile 100%")

#Κάνετε επίσης και τo αντίστοιχo συγκριτικό θηκόγραμμα

#Με βάση τα γραφήματα και τα αποτελέσματα των παραμέτρων σχολιάστε 
#την κατανομή της μεταβλητής age για κάθε ένα απο τα δείγματα.

boxplot(age~case, main="Age by Case", col=c("darkgreen","orange"), ylab="Age (years)", xlab="Case")

#Στη συνέχεια υποθέστε ότι η κατανομή της μεταβλητής age είναι κανονική (ανεξάρτητα από το σχόλιο σας στο 5). Τότε:

#  a. Κάνοντας χρήση του R, υπολογίστε με βάση το κάθε δείγμα το 99% διάστημα εμπιστοσύνης (δ.ε.) 
#για τη μέση τιμή της για κάθε group (Patients - Controls). 
#Συγκρίνετε και σχολιάστε τα δύο διαστήματα εμπιστοσύνης. Είναι «λογικό» το αποτέλεσμα;

t.test(age[case==0],age[case==1],conf.level=0.99)
t.test(age[case==0],conf.level=0.99)
t.test(age[case==1],conf.level=0.99)

#b. Ελέγξετε σε επίπεδο εμπιστοσύνης 95% με την βοήθεια του t-test τη μηδενική υπόθεση σύμφωνα με την οποία 
#η μέση ηλικία των Patients δεν διαφέρει απο την μέση ηλικία των Control, Τι συμπεράσματα μπορείτε να βγάλετε;

t<-t.test(age[case==0],age[case==1],conf.level=0.95)


######################### Παραδειγμα 2ο: #####################################################################

#Στη συνέχεια θα ασχοληθείτε με τις μεταβλητές του αρχείου "cats" αφού υποθέστε ότι η κατανομή της μεταβλητής 
#"Bwt" (body weight) είναι κανονική Τότε:


#a. Ελέγξετε σε επίπεδο εμπιστοσύνης 95% τη μηδενική υπόθεση σύμφωνα με την οποία το μέσο Bwt των Female 
#δεν διαφέρει από το μέσο Bwt των Male, Τι συμπεράσματα μπορείτε να βγάλετε;

require(MASS)
head(cats)
attach(cats)
t.test(Bwt[Sex=="M"],Bwt[Sex=="F"])

#b. Ποιο είναι το διάστημα εμπιστοσύνης 99% για την διαφορά στις μέσες τιμές του Bwt των δυο ομάδων;

t.test(Bwt[Sex=="M"],Bwt[Sex=="F"],conf.level=0.99)

#c. Κατασκευάστε το αντίστοιχο συγκριτικό θηκόγραμμα για το Bwt μεταξύ Male και Female.

boxplot(Bwt~Sex, main="Body Weight by Gender", col=c("red","lightblue"), ylab="Boby Weight", xlab="Gender")


#Κατασκευάστε το σημειόγραμμα (scatterplot) των μεταβλητών Hwt (άξονας Υ) και Bwt (άξονας Χ). 

plot(Bwt,Hwt, main="Heart Vs Body Weight", ylab="Heart Weight", xlab="Body Weight", col=c("red","blue"))

#Μπορείτε να κατασκευάσετε μοντέλο γραμμικής παλινδρόμησης που να προβλέπει την μεταβλητή Hwt γνωρίζοντας την Bwt? 

lm(Hwt~Bwt)

#Υπολογίστε τον συντελεστή συσχέτισης του Pearson για το προηγούμενο ερώτημα.

cor(Bwt, Hwt)



