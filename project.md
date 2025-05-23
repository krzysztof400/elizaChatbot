eliza-bot/
├── src/
│   ├── Main.hs                 -- Punkt wejścia
│   ├── Bot/Engine.hs          -- Silnik dopasowywania wzorców
│   ├── Bot/Memory.hs          -- Pamięć rozmowy
│   ├── Bot/Types.hs           -- Typy danych (np. BotState, UserFact)
│   └── Bot/Patterns.hs        -- Lista wzorców konwersacyjnych
├── data/
│   └── knowledge_base.json    -- Baza wiedzy (np. o roślinach)
├── test/
│   └── BotSpec.hs             -- Testy jednostkowe
├── README.md
├── stack.yaml / cabal.project -- Konfiguracja projektu
