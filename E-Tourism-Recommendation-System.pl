offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).




perm([H|T],L) :- perm(T,P), insert(H,P,L).       
perm([],[]).
%either insert first
insert(X,L,[X|L]).
%or insert later in the tail
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).


subset1([],[]).                                    
subset1([H|T],[H|T1]):- subset1(T,T1).
subset1([H|T],S):- subset1(T,S).


possibleSubset(L,R):- subset1(L,S), perm(S,R).            



subset2(L,S):-subset1(L,S), S\=[].                              


choosePreferences(Prefs, ChosenPreferences):-                                    
                                              subset2(Prefs,Y),
                                              helperpref(Y,ChosenPreferences).


helperpref([],[]).                                                                     
helperpref([activity(X)|T],[activity(Y)|T1]):-
                        subset2(X,Y),
                        helperpref(T,T1).
helperpref([H|T],[H|T1]):-   H\=activity(X),
                      helperpref(T,T1) .


preferenceSatisfaction(Offer, Customer,[],0).
preferenceSatisfaction(Offer, Customer,[accommodation(X)|T],S):-
                            customerPreferredAccommodation(Customer,X,S1),
                            preferenceSatisfaction(Offer,Customer,T,S2),
                            S is S1+S2.
preferenceSatisfaction(Offer, Customer,[means(X)|T],S):-
                             customerPreferredMean(Customer,X,S1),
                             preferenceSatisfaction(Offer,Customer,T,S2),
                             S is S1+S2.
preferenceSatisfaction(Offer,Customer,[activity([])|T],S):- preferenceSatisfaction(Offer,Customer,T,S).
preferenceSatisfaction(Offer,Customer,[activity([X|XT])|T],S):-
                                                                customerPreferredActivity(Customer,X,S1),
                                                                preferenceSatisfaction(Offer,Customer,[activity(XT)|T],S2),
                                                                S is S1+S2.
preferenceSatisfaction(Offer, Customer,[H|T],S):- H\=means(X),
                              H\= accommodation(Y),
                               H\=activity([]),
                              H\=activity(X),
                                 preferenceSatisfaction(Offer,Customer,T,S).


overlapPeriod(period(Year-Month-Day,Year1-Month1-Day1), period(Year2-Month2-Day2,Year3-Month3-Day3)):-                      
                      not(((Year1<Year2);(Year1==Year2,Month1<Month2);(Year1==Year2,Month1==Month2,Day1<Day2));
                      ((Year3<Year);(Year3==Year,Month3<Month);(Year3==Year,Month3==Month,Day3<Day))).



getOffer([],Offer):-offerMean(Offer,_).                                                                                            
getOffer([dest(DES)|T],Offer):- offerMean(Offer,_), Offer = offer(DES,_,_,_,_,_,_,_), getOffer(T,Offer).
getOffer([budget(BUD)|T],Offer):- offerMean(Offer,_), Offer = offer(_,_,BUDT,_,_,_,_,_),BUDT=<BUD, getOffer(T,Offer).
getOffer([means(M)|T],Offer):- offerMean(Offer,M), getOffer(T,Offer).
getOffer([accommodation(ACC)|T],Offer):- offerAccommodation(Offer,ACC), getOffer(T,Offer).
getOffer([period(PER,PER1)|T],Offer):- offerAccommodation(Offer,_),Offer = offer(_,_,_,_,_,period(OVER,OVER1),_,_),overlapPeriod(period(PER,PER1),period(OVER,OVER1)), getOffer(T,Offer).
getOffer([activity(ACT)|T],Offer):- offerAccommodation(Offer,_),Offer = offer(_,FULL,_,_,_,_,_,_),possibleSubset(FULL,ACT) ,getOffer(T,Offer).




recommendOfferForCustomer(Prefs, ChosenPrefs, O):- choosePreferences(Prefs,ChosenPrefs), getOffer(ChosenPrefs,O).



bringAllOffers(Set):- setof(Offers,offerMean(Offers,_),Set).


bringMostSatisfied(Customers, PreferenceList,List1, Offer,Customer,FinalValue):-
                                                                                bringMostSatisfiedhelper(Customers,PreferenceList,List,List1,Offer, MostSatisfied,Customer, 0,FinalValue).
bringMostSatisfiedhelper([], [],List,List,_, MostSatisfied,MostSatisfied,Initial,Initial).

bringMostSatisfiedhelper([HC|TC], [HP|TP],List,List1,Offer, MostSatisfied,Customer, Initial,FinalValue):-  preferenceSatisfaction(Offer, HC,HP,Satisfaction),
                                                                                                                Satisfaction>Initial ,
                                                          bringMostSatisfiedhelper(TC, TP,HP,List1, Offer, HC,Customer, Satisfaction,FinalValue).
bringMostSatisfiedhelper([HC|TC], [HP|TP],List,List1, Offer, MostSatisfied,Customer, Initial,FinalValue):-  preferenceSatisfaction(Offer, HC,HP,Satisfaction),
                                                                                                                Satisfaction=<Initial,
                                                          bringMostSatisfiedhelper(TC, TP,List,List1, Offer, MostSatisfied,Customer, Initial,FinalValue).


delete1(_,[],[]).
delete1(Item,[Item|T],T).
delete1(Item,[H|T],[H|NT]):- Item\=H, delete1(Item,T,NT).

bringListOfMostSatisfied([],_,_,_,[],_).
bringListOfMostSatisfied(_,_,_,offer(_,_,_,_,_,_,_,X),[],Counter):- Counter>=X.
bringListOfMostSatisfied(Customers,PreferenceList,Offer,offer(_,_,_,_,_,_,_,X), [Customer|FT],Counter):-
                                                                                Counter<X,
                                                                                Customers\=[],
                                                                                PreferenceList\=[],
                                            bringMostSatisfied(Customers, PreferenceList,List, Offer,Customer,_),
                                            delete1(List,PreferenceList,PreferenceListtemp),
                                            delete1(Customer,Customers,Customerstemp),
                                            Counter1 is Counter+1,
                                            bringListOfMostSatisfied(Customerstemp,PreferenceListtemp,Offer,Offer, FT,Counter1).






recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):- offerMean(Offer,_),
                                                bringListOfMostSatisfied(Customers,PreferenceList,Offer,Offer,CustomersChosen,0).



