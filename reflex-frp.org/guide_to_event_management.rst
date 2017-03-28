
Simple Reflex stuff guide, no DOM related stuff here.

https://www.reddit.com/r/reflexfrp/comments/3bocn9/how_to_extract_the_current_value_from_a_text_box/

Event is probably as you understand it, discrete events. Behavior's are values which change over time (but you don't know when they changed)
and a Dynamic is Event + Behavior, values which change over time, and you're notified when they change, too.
The problem with your example, is that omg is not an Event, Behavior or Dynamic but just a String (so it will never change).
What you might want to do is tag the event with the value from the text box like this:
omg <- mapDyn (\t -> "myUrl/" ++ t ++ "/me") value questionBox
dyn <- mkAsyncDyn "default" $ tag (current omg) insertEvent
This way omg is a Dynamic, so it can change over time. Then we tag the event with the value of the behavior current omg.
(Note that if we used directly tagDyn omg insertEvent the event would fire both when omg changed as well as when the button was clicked, which is not what we want)
mkAsyncDyn :: MonadWidget t m => T.Text -> Event t String -> m (Dynamic t (Maybe T.Text))
mkAsyncDyn defaultValue event = do
  ev <- performRequestAsync $ fmap (\url -> xhrRequest "GET" url def) event
  holdDyn (Just defaultValue) $ fmap _xhrResponse_body ev
So the takeaway here is that for values to update they need to be reactive type (Event, Behavior, Dynamic), sample is almost never what you want to do.


https://www.reddit.com/r/reflexfrp/comments/4nyteu/joindyn_and_eboth/
http://anderspapitto.com/posts/2016-11-09-efficient-updates-of-sum-types-in-reflex.html

Creating Event propagation graph
--------------------------------

mconcatDyn for monoid Dynamic
