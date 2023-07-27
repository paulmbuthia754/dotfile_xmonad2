{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}

module XMonad.Actions.CycleShiftRecentWS
        ( unView
        , cycleRecentWS_
        , cycleRecentNonEmptyWS_
        , cycleShiftRecentWS_
        , cycleShiftRecentNonEmptyWS_
        )
        where

import           XMonad hiding (workspaces)
import           XMonad.StackSet hiding (filter)
import           XMonad.Actions.CycleRecentWS
import qualified XMonad.StackSet                   as W
import qualified Data.List                         as L

import           Control.Arrow ((&&&))
import           Data.Function (on)

cycleRecentWS_ :: WorkspaceId -> [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentWS_ w = cycleWindowSets $ L.delete w . recentWS (const True)

cycleRecentNonEmptyWS_ :: WorkspaceId -> [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentNonEmptyWS_ w = cycleWindowSets $ L.delete w . recentWS ( not . null . W.stack)

cycleShiftRecentWS_ :: WorkspaceId -> [KeySym] -> KeySym -> KeySym -> X ()
cycleShiftRecentWS_ w = cycleShiftWindowSets $ L.delete w . recentWS (const True)

cycleShiftRecentNonEmptyWS_ :: WorkspaceId -> [KeySym] -> KeySym -> KeySym -> X ()
cycleShiftRecentNonEmptyWS_ w = cycleShiftWindowSets $ L.delete w . recentWS ( not . null . W.stack)

unView :: forall i l a s sd. (Eq i, Eq s) => StackSet i l a s sd -> StackSet i l a s sd -> StackSet i l a s sd
unView w0 w1 = fixOrderH . fixOrderV . view' (currentTag w0) $ w1
    where
        view' = if screen (current w0) == screen (current w1) then greedyView else view
        fixOrderV w | v : vs <- visible w = w{ visible = insertAt (pfxV (visible w0) vs) v vs }
                    | otherwise = w
        fixOrderH w | h : hs <- hidden w = w{ hidden = insertAt (pfxH (hidden w0) hs) h hs }
                    | otherwise = w
        pfxV = commonPrefix `on` fmap (tag . workspace)
        pfxH = commonPrefix `on` fmap tag

        insertAt :: Int -> x -> [x] -> [x]
        insertAt n x xs = let (l, r) = splitAt n xs in l ++ [x] ++ r

        commonPrefix :: Eq x => [x] -> [x] -> Int
        commonPrefix a b = length $ takeWhile id $ zipWith (==) a b

-- Reimplement cycleWindowSets with viewShift
cycleShiftWindowSets :: (WindowSet -> [WorkspaceId])
                     -> [KeySym]
                     -> KeySym
                     -> KeySym
                     -> X ()
cycleShiftWindowSets genOptions mods keyNext keyPrev = do
    (options, unView') <- gets $ (genOptions &&& unView) . windowset
    XConf {theRoot = root, display = d} <- ask
    let event = allocaXEvent $ \p -> do
            maskEvent d (keyPressMask .|. keyReleaseMask) p
            KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
            s <- keycodeToKeysym d c 0
            return (t, s)
    let setOption n = do 
            let ref = options `cycref` n
            windows $ W.shift ref
            windows $ W.view ref . unView'
            (t, s) <- io event
            case () of 
                () | t == keyPress && s == keyNext -> setOption (n+1)
                   | t == keyPress && s == keyPrev -> setOption (n-1)
                   | t == keyRelease && s `elem` mods -> return ()
                   | otherwise                        -> setOption n
    _ <- io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
    setOption 0
    io $ ungrabKeyboard d currentTime
    where
        cycref :: [r] -> Int -> r
        cycref l i = l !! (i `mod` length l)

