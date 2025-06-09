{-# LANGUAGE OverloadedStrings #-}
module Bot.Pattern where

import Bot.Types
import qualified Data.Text as T

moviePatterns :: [PatternRule]
moviePatterns = [
    -- Greetings - redirect to movies
    PatternRule "\\b(hello|hi|hey|good morning|good afternoon)\\b" "Hello! What's the last movie you watched?",
    PatternRule "\\b(goodbye|bye|see you|farewell)\\b" "Goodbye! Hope you watch something great soon!",

    -- Movie likes and preferences
    PatternRule "i like (.+) movies" "What is it about $1 movies that you enjoy?",
    PatternRule "i love (.+) movies" "What's your favorite $1 movie?",
    PatternRule "i enjoy (.+) films" "Can you recommend a good $1 film?",
    PatternRule "i like (.+)" "Is $1 a movie? Tell me about movies you like!",
    PatternRule "i love (.+)" "That sounds interesting! Have you seen any movies about $1?",

    -- Movie dislikes
    PatternRule "i hate (.+) movies" "What is it about $1 movies that you dislike?",
    PatternRule "i don't like (.+) movies" "Why don't $1 movies appeal to you?",
    PatternRule "i dislike (.+) films" "What would make a $1 film better for you?",
    PatternRule "i hate (.+)" "I see. Are there any movies that deal with $1 that you might enjoy?",

    -- Specific movies watched
    PatternRule "i watched (.+)" "How was $1? Would you recommend it?",
    PatternRule "i saw (.+)" "What did you think of $1? What was your favorite scene?",
    PatternRule "i've seen (.+)" "How would you rate $1? What genre was it?",
    PatternRule "i just watched (.+)" "Fresh from watching $1! What was the best part?",

    -- Movie genres
    PatternRule "\\b(action|thriller|horror|comedy|drama|romance|sci-fi|fantasy|documentary|animation)\\b" "What's your favorite $1 movie? Any recent $1 films you'd recommend?",
    PatternRule "what genre" "What movie genres do you usually enjoy? Action, comedy, drama?",

    -- Movie experiences and feelings
    PatternRule "i feel (.+)" "Have you seen any movies that made you feel $1?",
    PatternRule "i am (.+)" "Are there movies that make you feel $1? What are they?",
    PatternRule "i'm (.+)" "Do you watch movies when you're $1? What kind?",

    -- Movie recommendations and suggestions
    PatternRule "recommend (.+)" "For $1, I'd suggest exploring that genre! What similar movies have you enjoyed?",
    PatternRule "suggest (.+)" "What kind of $1 movies do you usually enjoy?",
    PatternRule "what should i watch" "What genre are you in the mood for? Comedy, drama, action?",

    -- Movie quality and ratings
    PatternRule "(.+) was good" "$1 sounds great! What made it good for you?",
    PatternRule "(.+) was bad" "What didn't work for you in $1? Poor plot, acting, or direction?",
    PatternRule "(.+) was amazing" "What made $1 so amazing? The story, visuals, or performances?",
    PatternRule "(.+) was terrible" "What made $1 terrible? Was it the script, acting, or something else?",

    -- Directors and actors
    PatternRule "\\b(director|directed by)\\b (.+)" "What do you think of $2's directing style? Have you seen their other films?",
    PatternRule "\\b(actor|actress)\\b (.+)" "What's your favorite movie with $2? Do you follow their career?",
    PatternRule "\\bstarring\\b (.+)" "How was $1's performance? Do you enjoy their other movies?",

    -- Movie theaters and watching experience
    PatternRule "\\b(theater|theatre|cinema)\\b" "Do you prefer watching movies in theaters or at home? What's the last movie you saw in theaters?",
    PatternRule "\\bnetflix\\b" "What's good on Netflix right now? Any hidden gems you've discovered?",
    PatternRule "\\bstreaming\\b" "What streaming service has the best movies? What are you watching there?",

    -- Movie series and franchises
    PatternRule "\\b(sequel|franchise|series)\\b" "Do you enjoy movie sequels? Which franchise do you follow?",
    PatternRule "part (\\d+)" "How does part $1 compare to the earlier films in the series?",

    -- Questions about movies
    PatternRule "(.+)\\?" "That's an interesting question about movies! What films come to mind when you think about $1?",
    PatternRule "why (.+)" "Speaking of $1, have you seen any movies that explore that theme?",
    PatternRule "how (.+)" "Interesting! Are there movies that show $1 well?",
    PatternRule "what (.+)" "When you think about $1, what movies come to mind?",

    -- Redirect non-movie topics to movies
    PatternRule "\\b(work|job|school|study)\\b" "That sounds busy! Do you watch movies to relax? What's your go-to genre?",
    PatternRule "\\b(family|friends)\\b" "Do you watch movies with your family or friends? What do you usually pick?",
    PatternRule "\\b(food|cooking)\\b" "There are some great food movies! Have you seen Chef or Julie & Julia?",
    PatternRule "\\b(travel|vacation)\\b" "Travel movies can be inspiring! Any favorite films set in exotic locations?",
    PatternRule "\\b(music|song)\\b" "Do you enjoy movie soundtracks? Any films with memorable music?",
    PatternRule "\\b(book|reading)\\b" "Do you prefer books or their movie adaptations? Any good book-to-film adaptations you've seen?",

    -- Yes/No responses - keep movie focused
    PatternRule "\\byes\\b" "Great! Tell me more about your movie preferences.",
    PatternRule "\\bno\\b" "I see. What kind of movies do you usually enjoy then?",
    PatternRule "\\bmaybe\\b" "Fair enough! What's the last movie that really impressed you?",

    -- Catch-all patterns - redirect to movies
    PatternRule "i (.+)" "Interesting that you $1. Do you watch movies about that topic?",
    PatternRule "you (.+)" "Let's focus on you and movies! What's your all-time favorite film?",
    PatternRule ".*" "Let's talk about movies! What have you watched recently?"
    ]

-- Movie-focused default responses
-- | List of default responses for movies
movieDefaultResponses :: [T.Text]
movieDefaultResponses =
    [ "Can you tell me more about the movies you like?"
    , "What kind of movies do you enjoy watching?"
    , "Do you have a favorite movie or genre?"
    , "I'd love to hear about your favorite films!"
    , "What was the last movie you watched?"
    ]