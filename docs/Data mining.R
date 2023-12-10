library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(mice)
library(tidyr)
library(gridExtra)


film<-read_csv("film.csv",na = "NULL"); str(film); table(is.na(film))
film_category<-read_csv("film_category.csv"); str(film_category); table(is.na(film_category))
category<-read_csv("category.csv"); str(category); table(is.na(category))
inventory<-read_csv("inventory.csv"); str(inventory); table(is.na(inventory))
rental<-read_csv("rental.csv",na = "NULL"); str(rental); table(is.na(rental))
payment<-read.csv("payment.csv"); str(payment); table(is.na(payment))
film_actor<-read.csv("film_actor.csv"); str(film_actor); table(is.na(film_actor))
actor<-read.csv("actor.csv"); str(actor); table(is.na(actor))
customer<-read.csv("customer.csv"); str(customer); table(is.na(customer))
address<-read.csv("address.csv"); str(address); table(is.na(address))
city<-read.csv("city.csv"); str(city); table(is.na(city))
country<-read.csv("country.csv"); str(country); table(is.na(country))



# Punto 1
film_category %>% 
  left_join(category, by=("category_id"="category_id")) %>% 
  ggplot(mapping=aes(name))+
  geom_bar()+
  labs(
    title = "Distribuzione dei film in base al genere",
    x = "Categoria film",
    y = "Frequenza assoluta"
  )

# Punto 2
film %>% left_join(film_category, by=("film_id"="film_id")) %>% 
  left_join(category, by=("category_id"="category_id")) %>% 
  right_join(inventory, by=("film_id"="film_id")) %>%
  right_join(rental, by=("inventory_id"="inventory_id")) %>%
  group_by(name) %>% 
  summarise(Frequenza = n()) %>%
  arrange(desc(Frequenza)) %>%
  filter(Frequenza>=Frequenza[3])

film %>% left_join(film_category, by=("film_id"="film_id")) %>% 
  left_join(category, by=("category_id"="category_id")) %>% 
  right_join(inventory, by=("film_id"="film_id")) %>%
  right_join(rental, by=("inventory_id"="inventory_id")) %>%
  group_by(name) %>%
  summarise(Frequenza = n()) %>%
  arrange(desc(Frequenza)) %>%
  ggplot(aes(reorder(name, -Frequenza), Frequenza)) +  
  geom_col() +
  labs(
    title = "Distribuzione dei film noleggiati in base alla categoria",
    subtitle = "in ordine decrescente",
    x = "Categoria film",
    y = "Frequenza assoluta"
  )


# Punto 3  
film %>% left_join(film_category, by=("film_id"="film_id")) %>% 
  left_join(category, by=("category_id"="category_id")) %>% 
  right_join(inventory, by=("film_id"="film_id")) %>%
  right_join(rental, by=("inventory_id"="inventory_id")) %>%
  right_join(payment, by=("rental_id"="rental_id")) %>%
  group_by(name) %>% 
  summarise(Costo = sum(amount)) %>%
  arrange(Costo) %>%
  filter(Costo<=Costo[3])

film %>% left_join(film_category, by=("film_id"="film_id")) %>% 
  left_join(category, by=("category_id"="category_id")) %>% 
  right_join(inventory, by=("film_id"="film_id")) %>%
  right_join(rental, by=("inventory_id"="inventory_id")) %>%
  right_join(payment, by=("rental_id"="rental_id")) %>%
  group_by(name) %>% 
  summarise(Costo = sum(amount)) %>%
  arrange(Costo) %>%
  ggplot(aes(reorder(name, Costo), Costo)) +  
  geom_col() +
  labs(
    title = "Distribuzione del costo di noleggio in base alla categoria",
    subtitle = "in ordine crescente",
    x = "Categoria film",
    y = "Frequenza assoluta"
  )
  
  
# Punto 4
film %>% anti_join(inventory, by=("film_id"="film_id")) %>%
  select(title) %>% View()

# Punto 5
film %>% left_join(film_category, by=("film_id"="film_id")) %>% 
  left_join(category, by=("category_id"="category_id")) %>% 
  right_join(film_actor, by=("film_id"="film_id")) %>% 
  left_join(actor, by=("actor_id"="actor_id")) %>%
  filter(name=="Drama") %>%
  unite("Attore",first_name, last_name, sep=" ") %>% 
  unite("Attore", actor_id, Attore, sep=" - ") %>%
  group_by(Attore) %>%
  summarise(Frequenza = n()) %>%
  arrange(desc(Frequenza)) %>%
  filter(Frequenza >= Frequenza[3])

film %>% left_join(film_category, by=("film_id"="film_id")) %>% 
  left_join(category, by=("category_id"="category_id")) %>% 
  right_join(film_actor, by=("film_id"="film_id")) %>% 
  left_join(actor, by=("actor_id"="actor_id")) %>%
  filter(name=="Drama") %>%
  unite("Attore",first_name, last_name, sep=" ") %>% 
  unite("Attore", actor_id, Attore, sep=" - ") %>%
  group_by(Attore) %>%
  summarise(Frequenza = n()) %>%
  arrange(desc(Frequenza)) %>%
  head(8) %>%
  ggplot(aes(reorder(Attore, -Frequenza), Frequenza)) +  
  geom_col() +
  labs(
    title = 'Distribuzione degli attori presenti nei film di categoria "Drama"',
    subtitle = "in ordine decrescente",
    x = "Attore",
    y = "Frequenza assoluta"
  )
  
# Punto 6
customer %>% left_join(address, by=("address_id"="address_id")) %>%
  left_join(city, by=("city_id"="city_id")) %>%
  left_join(country, by=("country_id"="country_id")) %>%
  group_by(country) %>%
  summarise(Frequenza = n()) %>%
  arrange(desc(Frequenza)) %>%
  filter(Frequenza>=Frequenza[5])
  
customer %>% left_join(address, by=("address_id"="address_id")) %>%
  left_join(city, by=("city_id"="city_id")) %>%
  left_join(country, by=("country_id"="country_id")) %>%
  group_by(country) %>%
  summarise(Frequenza = n()) %>%
  arrange(desc(Frequenza)) %>%
  head(15) %>%
  ggplot(aes(reorder(country, -Frequenza), Frequenza)) +  
  geom_col() +
  labs(
    title = "Distribuzione clientela per Paese",
    subtitle = "Ordine decrescente",
    x = "Paese",
    y = "Frequenza assoluta"
  )
  
# Punto 7
punto7 <- film %>% right_join(inventory, by=("film_id"="film_id")) %>%
  right_join(rental, by=("inventory_id"="inventory_id")) %>%
  mutate(Intervallo = interval(start=rental_date, end=return_date), Durata=time_length(Intervallo, unit="days")) %>%
  select(title, inventory_id, customer_id, staff_id, rental_id, Durata)
  
md.pattern(punto7)

imp_1 <- punto7 %>% mice(seed=12345, method="mean") %>% 
  complete()
imp_2 <- punto7 %>% mice(seed=12345, method="norm.predict") %>% 
  complete()
imp_3 <- punto7 %>% mice(seed=12345, method="norm.nob") %>% 
  complete()
imp_4 <- punto7 %>% mice(seed=12345, method="pmm") %>% 
  complete() 

base <- punto7 %>% ggplot(aes(Durata)) +
  geom_density(fill=2, alpha=0.2) +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,0.14))

plot_1 <- base +
  geom_density(aes(imp_1$Durata), fill=3, alpha=0.2) +
  labs(
    title = 'method = "mean"'
  )
plot_2 <- base + 
  geom_density(aes(imp_2$Durata), fill=4, alpha=0.2) +
  labs(
    title = 'method = "norm.predict"'
  )
plot_3 <- base +
  geom_density(aes(imp_3$Durata), fill=5, alpha=0.2) +
  labs(
    title = 'method = "norm.nob"'
  )
plot_4 <- base +
  geom_density(aes(imp_4$Durata), fill=6, alpha=0.2) +
  labs(
    title = 'method = "pmm"'
  )
grid.arrange(plot_1, plot_2, plot_3, plot_4, nrow = 2, ncol = 2)

imp_4 %>% group_by(title) %>%
  summarise(Durata_media = mean(Durata))

primi10 <- imp_4 %>% group_by(title) %>%
  summarise(Durata_media = mean(Durata)) %>%
  arrange(desc(Durata_media)) %>%
  head(10) %>%
  ggplot(aes(reorder(title, -Durata_media), Durata_media)) +  
  geom_col() +
  labs(
    title = "Distribuzione della durata media di noleggio per titolo",
    subtitle = "primi 10",
    x = "Titolo",
    y = "Durata media noleggio"
  )

ultimi10 <- imp_4 %>% group_by(title) %>%
  summarise(Durata_media = mean(Durata)) %>%
  arrange(Durata_media) %>%
  head(10) %>%
  ggplot(aes(reorder(title, -Durata_media), Durata_media)) +  
  geom_col() +
  labs(
    title = "Distribuzione della durata media di noleggio per titolo",
    subtitle = "ultimi 10",
    x = "Titolo",
    y = "Durata media noleggio"
  )

grid.arrange(primi10, ultimi10, nrow=2, ncol=1)

