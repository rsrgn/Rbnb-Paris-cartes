import pandas as pd
DATA_FOLDER = "Downloads/"
# Data from InsideAirBNB

df_calendar = pd.read_csv(DATA_FOLDER + 'calendar.csv')
df_listing = pd.read_csv(DATA_FOLDER + 'listings.csv')

df_calendar.columns = ['id', 'date', 'available', 'price']
df_calendar['date'] = pd.to_datetime(df_calendar['date'], format='%Y-%m-%d')

df_calendar = df_calendar.loc[df_calendar['available'] == 'f']

agg_duration = df_calendar.groupby(['id']).size().reset_index()
agg_duration.columns = ['id', 'duration']

df_final = df_listing.merge(agg_duration, on='id', how='left')
df_final['price'] = df_final['price'].str[1:]
df_final['price'] = df_final['price'].str.replace(',','')
df_final['gain_annuel'] = df_final['price'].astype('float16') * df_final['duration']
df_final.dropna(subset=['name',
                        'host_name',
                        'city',], inplace=True)
df_final[['id', 'name', 'host_id', 'host_name','city',
          'country_code', 'country', 'latitude', 'longitude',
          'property_type', 'room_type',
          'accommodates','price',
          'minimum_nights',
          'availability_365', 'number_of_reviews',
          'calculated_host_listings_count','reviews_per_month',
          'duration', 'gain_annuel']].to_csv(DATA_FOLDER + 'airbnb.csv', sep=";", index=False)
# Data from DataGouv
df_loyer = pd.read_csv(DATA_FOLDER + 'encadrement_loyers.csv')
df_loyer = df_loyer.loc[(df_loyer['ville'] == 'PARIS') &
                        (df_loyer['annee'] >= 2017) & 
                        (df_loyer['meuble'] == 1)]
df_loyer_par_quartier = df_loyer.groupby(['nom_quartier'])['ref'].mean().reset_index()
df_loyer_par_quartier['nom_quartier'] = df_loyer_par_quartier['nom_quartier'].str.lower() \
                                                    .str.normalize('NFKD') \
                                                    .str.encode('ascii', errors='ignore') \
                                                    .str.decode('utf-8')
old_values = ['saint-',
             'sainte-',
             'faubourg-',
             'avoie',
             '^notre-dame-',
             '^parc-de-',
             ' 15art',
             '^la ',
             'plaine de ']
new_values = ['st-',
             'ste-',
             'fg-',
             'avoye',
             'nd-',
             'parc-',
             '',
             '',
             'plaine-']
df_loyer_par_quartier['nom_quartier'] = df_loyer_par_quartier['nom_quartier'].replace(old_values,
                                                                            new_values,
                                                                            regex=True)
# Data from OpenStreetMap
df_quartier = pd.read_csv(DATA_FOLDER + 'osm_paris_quartiers.csv', sep=";", usecols=["osm_id", "name",
                                     "addr.postcode", "admin_level",
                                     "alt_name", "boundary",
                                     "ref.INSEE", "short_name", "type",
                                     "wikidata", "wikipedia"])

df_quartier.loc[df_quartier['addr.postcode'].isnull(), 'addr.postcode'] = df_quartier.loc[df_quartier['addr.postcode'].isnull(), 'ref.INSEE'].astype(str).replace(r"^751","750", regex=True).str[:-2]
df_quartier['short_name'] = df_quartier['short_name'].str.lower() \
                                                    .str.normalize('NFKD') \
                                                    .str.encode('ascii',
                                                                errors='ignore') \
                                                    .str.decode('utf-8')
df_quartier['short_name'] = df_quartier['short_name'].replace([r'^le ', r'^la ', r'les '],
                                                              '',
                                                              regex=True)
old_values = ['enfants rouges',
             r' des ',
             'palais royal',
             'vendome',
             r'monceau']
new_values = ['enfants-rouges',
             '-des-',
             'palais-royal',
             'place-vendome',
             'monceaux']
df_quartier['short_name'] = df_quartier['short_name'].replace(
                                                        old_values,
                                                        new_values,
                                                        regex=True)
df_quartier = df_quartier.rename(columns={'short_name':'nom_quartier'})
df_quartier.loc[df_quartier['nom_quartier'].isnull(), 'nom_quartier'] = 'fg-du-roule'
df_quartier.loc[df_quartier['nom_quartier'].str.contains('iles'), 'nom_quartier'] = "notre-dame"
df_quartier['c_qu'] = df_quartier['ref.INSEE'].astype(str).str[-2:]
df_quartier.merge(df_loyer_par_quartier, on="nom_quartier").to_csv(DATA_FOLDER + 'loyer_par_quartier.csv',
                                                                   sep=";", index=False)
population = pd.read_csv(DATA_FOLDER + 'population_paris/ensemble/communes.csv', sep=";")
population.columns = population.iloc[6]
population = population.iloc[7:]
population = population.loc[population['Code département'] == "75"]
population = population[['Nom de la commune', 'Population totale']]
population = population.replace(r'\D', '', regex=True)
population.columns = ['nom_commune', 'pop_totale']
population['nom_commune']= population['nom_commune']+str("ème")
population = population.replace("1ème", "1er")
population['nom_commune'] = population['nom_commune'] + str(' Ardt')
population.to_csv(DATA_FOLDER + 'population_paris.csv')

df_listings2 = df_listing[['host_id',
                           'calculated_host_listings_count',
                           'availability_365', 'price', 'minimum_nights', 'reviews_per_month']].copy(deep=True)
df_listings2['price'] = df_listings2['price'].str[1:]
df_listings2['price'] = df_listings2['price'].str.replace(',','')

# Hosts with < 2 properties
df_listings2 = df_listings2.loc[(df_listings2['calculated_host_listings_count'] > 1) &
                               (df_listings2['availability_365'] > 120)
                               ]
df_listings2.sort_values('calculated_host_listings_count', ascending=False, inplace=True)
df_listings2['estimated_income'] = df_listings2['price'].astype('float16') * df_listings2['minimum_nights'] * df_listings2['reviews_per_month']

host_monthly_income = df_listings2.groupby('host_id')['estimated_income'].sum().reset_index()
host_monthly_income.columns = ['host_id', 'estimated_monthly_income']

df_listings2.to_csv(DATA_FOLDER + 'possibly_illegal_hosts.csv', sep=";", index=False)
host_monthly_income.to_csv(DATA_FOLDER + 'possibly_illegal_hosts_monthly_income.csv', sep=";", index=False)