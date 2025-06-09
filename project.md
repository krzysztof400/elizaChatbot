# ELIZA Chatbot
### Krzysztof Zając i Wit Kowal

## Opis
Eliza to prosty chatbot napisany w Haskellu, który prowadzi rozmowę o filmach na podstawie zdefiniowanych wzorców tekstowych. Wykorzystuje wyrażenia regularne, losowy wybór odpowiedzi i działa w terminalu w trybie REPL. Kod ma modularną strukturę, co ułatwia dodawanie nowych reguł i rozwój bota. 

## Struktura
```
eliza-bot/
├── src/
│   ├── Main.hs                -- Punkt wejścia
│   ├── Bot/Engine.hs          -- Silnik dopasowywania wzorców
│   ├── Bot/Memory.hs          -- Pamięć rozmowy
│   ├── Bot/Types.hs           -- Typy danych (np. BotState, UserFact)
│   └── Bot/Patterns.hs        -- Lista wzorców konwersacyjnych
├── data/
│   └── knowledge_base.json    -- Baza wiedzy (o filmach)
├── test/
│   └── BotSpec.hs             -- Testy jednostkowe
├── README.md
└── stack.yaml / cabal.project -- Konfiguracja projektu
```

## Używane biblioteki
- base - Standardowa biblioteka Haskella
- regex-compat - Dopasowywanie wzorców
- random - Losowy wybór odpowiedzi
- text - Wydajne operacje na tekstach
- containers - Do struktury pamięci (np. Map)
- tasty - Framework do testów

## Podział obowiązków
- I/O, typy, konfiguracja - wspólnie
- Silnik, wzorce - Krzysztof
- Pamięć, testy, baza wiedzy + pomoc z silnikiem i wzorcami - Wit

## Plan pracy
- 1 tydzień - I/O, typy konfiguracja, ustalenie wzorców i typów danych - robione wspólnie 
- 2 i 3 tydzień - implementacja swojej części projektu
