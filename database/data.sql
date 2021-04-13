insert into transport.place
    (coords, friendly_name, additional_description, place_type)
values
    (ST_SETSRID(ST_MakePoint(19.99088466,50.08418097),4326), 'Lotnisko Czyżyny', 'Dostawa samolotów', 0),
    (ST_SETSRID(ST_MakePoint(19.98225619084028,50.087059455968785),4326), 'Krokus', 'Centrum Handlowe', 0),
    (ST_SETSRID(ST_MakePoint(19.99714226,50.08581596),4326), 'Pizza', 'tu jest dobra pizza', 0),
    (ST_SETSRID(ST_MakePoint(19.9348721,50.0626424),4326), 'Piwo', 'jakieś tam piwo', 0),
    (ST_SETSRID(ST_MakePoint(19.93926,50.06003),4326), 'SuperPiwo', 'Najlepsze piwo w Krakowie', 0),
    (ST_SETSRID(ST_MakePoint(19.99525,50.03373),4326), 'Nic', 'Nic tu nie ma', 0),
    (ST_SETSRID(ST_MakePoint(19.999951494324893,50.06395076826511),4326), 'M1 Kraków', 'Zakupy i galeria', 0)

-- vim:et
