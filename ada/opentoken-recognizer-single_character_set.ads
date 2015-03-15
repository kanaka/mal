-------------------------------------------------------------------------------
--
-- Copyright (C) 1999 FlightSafety International and Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  This software was originally developed by the following company, and was
--  released as open-source software as a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000
--
-------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Characters.Latin_1;

-----------------------------------------------------------------------------
--  This package implements a token recognizer for a token of any
--  number of characters from a given set. The most useful use of this
--  facility in a typical application is locating strings of
--  "whitespace"
-----------------------------------------------------------------------------
package OpenToken.Recognizer.Single_Character_Set is

   type Instance is new OpenToken.Recognizer.Instance with private;

   --------------------------------------------------------------------------
   --  This procedure will be called to create a Character_Set token.
   --  Set should be given a string containing all the characters that
   --  can be the single character token. The Set may be created using the
   --  operations in Ada.Strings.Maps.
   --
   --------------------------------------------------------------------------
   function Get (Set : in Ada.Strings.Maps.Character_Set) return Instance;

private


   type State_ID is (First_Char, Done);

   type Instance is new OpenToken.Recognizer.Instance with record

      --  The finite state machine state
      State : State_ID := First_Char;

      Set   : Ada.Strings.Maps.Character_Set;

   end record;

   overriding procedure Clear (The_Token : in out Instance);

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict);

end OpenToken.Recognizer.Single_Character_Set;
