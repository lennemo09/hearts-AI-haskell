-- Strategies collected from these sources:
-- https://viphearts.com/blog/hearts-tips/
-- https://en.wikibooks.org/wiki/Card_Games/Hearts/Strategy
-- http://mark.random-article.com/hearts/advanced.html
-- https://www.thesprucecrafts.com/hearts-card-game-strategy-and-tips-411726

-----------------------------------------------------------------------------
module Player (
    playCard,
    makeBid
)
where

-- Basic imports
import Hearts.Types
import Cards

------------------------------------------------
---- | | | | MAIN playCard FUNCTION | | | | ----
------------------------------------------------
playCard :: PlayFunc
-- | PATTERN 1 | Leading first trick of the round -> Always C2, assuming the system calls playCard correctly
playCard _ _ [] Nothing = (Card Club Two, "[]")

-- | PATTERN 2 | First trick of round but not leading
playCard _ hand _ Nothing
  -- CASE 1:
  -- Since for first round, lead is club, if hand has clubs, play the largest club.
   | hasClubs hand = (maximum  clubs, "[]") -- Play largest club

  -- CASE 2:
  -- If you have no clubs, this is opportunity to get rid of any unwated cards.
  -- Check if you only have point cards.
  -- If you do, try to trash the `most risky` card in the order of riskiness: Spade Queen, Space Ace, Spade King,
  -- or Hearts.
  -- But since in this case where you only have point cards, the only risky cards are Spade Queen and Hearts
   | allPoints hand && (Card Spade Queen) `elem` hand = (Card Spade Queen, "[]")  -- If have SQ, trash it
   | allPoints hand = (maximum hand, "[]")  -- Else, play highest Hearts

  -- CASE 3:
  -- If you have cards outside of point cards and clubs, you may have other risky cards.
   | (Card Spade Ace) `elem` hand = (Card Spade Ace, "[]")  -- Trash SA or SK
   | (Card Spade King) `elem` hand = (Card Spade King, "[]")

   -- CASE 4:
   -- My strategy here is a pretty common one, where you try to void yourself of diamonds and clubs first, so you can
   -- focus on dealing with other risky suits.
   -- Attempt to void  Diamonds (since you don't have clubs).
   -- When free to dump a card, always try to dump large ones since they are most likely to take a trick.
   | otherwise = (maximum diamonds, "[]")

   where
    clubs = getCardsOfSuit hand Club
    diamonds = getCardsOfSuit hand Diamond

-- | PATTERN 3 | Arbitrary trick, but you're the leading player.
playCard _ hand [] (Just (prevTrick, memory)) -- Leading any trick
   -- CASE 1:
   -- Queen of Spades has been played, leadWithNoQueens strat
   -- Because Queen of Spades has been played already, you don't need to play small clubs to force the Queen of Spades
   -- out any more. Try to void of clubs or diamonds.
   -- Due to the action of voiding clubs or diamonds being quite a chore (not complex but lengthy),
   -- I made a seperate function for this case.
   | queenPlayed prevTrick memory = leadWithNoQueens hand prevTrick memory

   -- CASE 2:
   -- Queen of Spades has not been played
   -- Try to lead with small spades strat, to force out the Queen.
   | hasSafeSpades = (minimum spades, outputMemory)

   -- CASE 3:
   -- If queen is not played and void of spades, lead with smallest cards to avoid taking queen or hearts.
   -- Note: mininum will not play a heart card except when you MUST play hearts (all hearts).


   -- This raised an error when hand is only hearts and risky cards, thus the subcase where you are forced to lead
   -- with a risky card is added (the one above).
   | hasRisky && mustPlayRisky = (maximum risky, outputMemory)
   | hasSpades && hasNotSpades = (minimum notSpades, outputMemory)
   | otherwise = (minimum hand, outputMemory) -- Only have spades
    where
      spades = getCardsOfSuit hand Spade
      notSpades = getCardsNotOfSuit hand Spade
      hasNotSpades = length notSpades > 0
      hasSpades = length spades > 0
      hasSafeSpades = spades /= (getRiskyCards hand)
      risky = getRiskyCards hand
      hasRisky = length risky > 0
      hearts = getCardsOfSuit hand Heart
      heartsAndRisky = risky ++ hearts
      mustPlayRisky = length heartsAndRisky == length hand
      outputMemory = makeMemory prevTrick memory

-- | PATTERN 4 | Arbitrary trick, but you're a following player.
playCard _ hand currentTrick (Just (prevTrick, memory)) -- Playing any trick
   -- Case 1:
   -- Lead suit is Spade and you can follow.
   -- If you have spades, then you must follow but play the smallest spades (as usual to not take the trick).
   -- I initially implemented where you check if there is a spades on your hand that is not the smallest spades you
   -- have, but all the spades smaller than it has already been played or on your hand, then play that spade.
   -- Though I realize that since you have the smallest spades in the round anyway, it doesn't matter if you play the
   -- smallest one or a larger one, they're still the smallest spades to other players -> I left the redundant code out.
   | lead == Spade && hasSpades = (minimum spades, outputMemory)

   -- Case 2:
   -- Lead is Spade, but you can't follow.
   -- Which gives you a free chance to get rid of any card. -> Play the largest one that is not spades.
   -- (maximum returns largest Heart if they have any). You don't have spades so risky cards aren't a problem here.
   | lead == Spade = (maximum notSpades, outputMemory) -- Can't follow spade

   -- Case 3:
   -- Lead is not Spade and you can follow and you MAY have to take this trick.
   -- If you feel like you have to take a trick, take it with your highest card.
   -- The function mayTakeTrick deals with estimating if you need to take a trick (logic will be in its comments).
   | canFollow && (mayTakeTrick followCards playedCardsWithLeadSuit) = (maximum followCards, outputMemory)

   -- Case 4:
   -- If you feel safe, just play a smallest follow suit as usual.
   | canFollow = (minimum followCards, outputMemory)

   -- Case 5:
   -- Can't follow, try to trash risky cards or highest hearts.
   | haveQueen hand = (Card Spade Queen, outputMemory)
   | (Card Spade Ace) `elem` hand = (Card Spade Ace, outputMemory)
   | (Card Spade King) `elem` hand = (Card Spade King, outputMemory)
   | otherwise = (maximum hand, outputMemory)

   where
    lead = getLeadSuit currentTrick
    followCards = getFollowCards hand currentTrick
    canFollow = length followCards > 0
    spades = getCardsOfSuit hand Spade
    notSpades = getCardsNotOfSuit hand Spade
    hasSpades = length spades > 0
    playedCardsWithLeadSuit = getCardsOfSuit (cardsFromTuples currentTrick) lead
    outputMemory = makeMemory prevTrick memory


-- PATERN 3 \ CASE 1 SUB-CASE HANDLER
leadWithNoQueens :: [Card] -> [(Card, PlayerId)] -> String -> (Card, String)
leadWithNoQueens hand prevTrick memory
   -- If you have less clubs than diamonds, void clubs
   | hasClubs hand && lessClubs = (minimum clubs, outputMemory)
   -- If you have less diamonds than clubs, void diamonds
   | hasDiamonds =  (minimum diamonds, outputMemory)
   -- Else, just play the smallest card you have
   | otherwise = (minimum hand, outputMemory)
   where
    lessClubs = (length $ clubs) < (length $ diamonds)
    hasDiamonds = length diamonds > 0
    clubs = getCardsOfSuit hand Club
    diamonds = getCardsOfSuit hand Diamond
    outputMemory = makeMemory prevTrick memory

-------------------------------------------------
---- | | | | GLOBAL HELPER FUNCTIONS | | | | ----
-------------------------------------------------
-- NOTE: For some functions, the attempted point-free form which is I personally find quite unpleasing to read will be
--       added as a comment.
--       For point-free functions, the original form is left as a comment, just in case it is easier to understand.

-- | mayTakeTrick:
--   This function takes in the cards you have on hand to follow the current suit, the cards played played
--   in the current trick.
--   If the smallest cards you can play is larger than the cards on the current trick, you are likely to take that
--   trick. (though this is only accurate if you're the last or slightly acurrate if you're the second to last player).
mayTakeTrick :: [Card] -> [Card] -> Bool
-- mayTakeTrick followCards playCards = (minimum followCards) > (maximum playCards)
mayTakeTrick followCards playCards
   | length playCards > 2 = (minimum followCards) > (maximum playCards)
   | otherwise = False
-------------------------------------------------

-- | queenPlayed:
--   This functions checks if the Queen has been played before the current trick by searching for it in the memory.
--   From looking at the documentation for `elem` at:
--   https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldMap
--   It appears to be implemented using any (which uses foldMap), which makes it a linear search.
--   Though because of the small size of the problem (any list I have is at most of length 52), I find linear search to
--   be efficient enough.
queenPlayed :: [(Card, PlayerId)] -> String -> Bool
-- queenPlayed = ((Card Spade Queen `elem`) .) . readMemory
queenPlayed prevTrick memory = (Card Spade Queen) `elem` readMemory prevTrick memory
-------------------------------------------------

-- | cardsFromTuples:
--   Self-explainatory, uses to parse currentTrick and previousTrick and ignore playerIDs, only cards.
cardsFromTuples :: [(Card, PlayerId)] -> [Card]
--cardsFromTuples tuple = map (\(c,_) -> c) tuple
cardsFromTuples = map fst
-------------------------------------------------

-- | haveQueen:
--   Straight forward function, also uses `elem` to find Queen in hand.
haveQueen :: [Card] -> Bool
-- haveQueen hand = (Card Spade Queen) `elem` hand
haveQueen = (Card Spade Queen `elem`)
-------------------------------------------------

-- | getLeadSuit:
--   This function checks for the starting card of the current trick and get its suit.
getLeadSuit :: [(Card, PlayerId)] -> Suit
-- getLeadSuit currentTrick = getSuit $ last $ cardsFromTuples currentTrick
getLeadSuit = getSuit . last . cardsFromTuples
-------------------------------------------------

-- | getFollowCards:
--   This function calls the previous function to get the current trick, and get the cards on hand with that suit.
getFollowCards :: [Card] -> [(Card, PlayerId)] -> [Card]
--getFollowCards hand currentTrick = filter (\card -> (getSuit card ==) $ getLeadSuit currentTrick) hand
getFollowCards hand currentTrick = filter ((getLeadSuit currentTrick ==) . getSuit) hand
-------------------------------------------------

-- | getSuit:
--   Get a suit from a Card context.
getSuit :: Card -> Suit
getSuit (Card s _) = s
-------------------------------------------------

-- | getCardsOfSuit:
--   Get cards with given suit from given hand.
getCardsOfSuit :: [Card] -> Suit -> [Card]
--getCardsOfSuit hand suit = filter (\card -> getSuit card == suit) hand
getCardsOfSuit hand suit = filter ((suit ==) . getSuit) hand
-------------------------------------------------

-- | getCardsNotOfSuit:
--   Reverse of getCardsOfSuit.
getCardsNotOfSuit :: [Card] -> Suit -> [Card]
--getCardsNotOfSuit hand suit = filter (\card -> getSuit card /= suit) hand
getCardsNotOfSuit hand suit = filter ((suit /=) . getSuit) hand
-------------------------------------------------

-- | getPoints:
--   Get point cards, which get Hearts and Spade Queen.
getPoints :: [Card] -> [Card]
--getPoints hand = getCardsOfSuit hand Heart ++ filter (Card Spade Queen ==) hand
getPoints = ((++) . flip getCardsOfSuit Heart) <*> (filter (Card Spade Queen ==))
-------------------------------------------------

-- | getRiskyCards:
--   Get risky cards, which are King and Ace of Spades.
getRiskyCards :: [Card] -> [Card]
--getRiskyCards hand = filter (>= Card Spade Queen) (getCardsOfSuit hand Spade)
getRiskyCards = filter (>= Card Spade Queen) . flip getCardsOfSuit Spade
-------------------------------------------------

-- | hasClubs:
--   Self explainatory, check if hand has clubs. I made this global because it is used commonly.
hasClubs :: [Card] -> Bool
-- hasClubs hand = (> 0) $ length (getCardsOfSuit hand Club)
hasClubs = (> 0) . length . flip getCardsOfSuit Club
-------------------------------------------------

-- | allPoints:
--   Check if hand consists of only point cards.
allPoints :: [Card] -> Bool
-- allPoints = ((==) . length . getPoints) <*> length
-- This point-free function is acceptable, but I see the original version just makes much more sense.
-- So I chose to not make this simple and intuitive function to be more complicated than it needed to be just for the
-- sake of making it a higher-order function.
allPoints hand = (length $ getPoints hand) == (length hand)
-------------------------------------------------

-- | readMemory:
--   This function takes the string from memory, parse it into a list, and append it to a list of cards from the
--   previous trick (which was process by cardsFromTuples).
readMemory :: [(Card, PlayerId)] -> String -> [Card]
--readMemory = (. read) . (++) . cardsFromTuples
readMemory prevTrick memory = cardsFromTuples prevTrick ++ read memory
-------------------------------------------------

-- | makeMemory:
--   This function uses readMemory to get the previous memory + previous trick,
--   to create a new memory string for output.
makeMemory :: [(Card, PlayerId)] -> String -> String
--makeMemory = show . readMemory doesn't work? I'm still confused for this super simple one.
makeMemory prevTrick memory = show $ readMemory prevTrick memory
-------------------------------------------------

---------------------------------------
-- | | | | END OF ASSIGNMENT | | | | --
---------------------------------------


makeBid :: BidFunc
makeBid = undefined

-- ############################################## --
