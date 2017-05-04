{-
	The goal of this small program is to manage a text calendar. 
	FEATURES
		- Copy upcoming appointments to current.text (we can overwrite current 
		  each time)
		- Move old appointments from calendar.text and current.text to 
		  past_YYYY.text except for repeating apointments which are updated as 
		  the pass.
-}

-- Pseudocode for the parser
-- header >> loop parseInterval indent=0 | parsePoint indent=0
-- parsePoint n = (n tab) >> dateTime >> body >> ?note >> repeat >> newLine >> loop parsePoint (n+1) 
