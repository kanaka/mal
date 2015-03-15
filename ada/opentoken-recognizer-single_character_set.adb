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

-----------------------------------------------------------------------------
--  This package implements a token recognizer for a token of a
--  single characters from a given set.
-----------------------------------------------------------------------------
package body OpenToken.Recognizer.Single_Character_Set is

   overriding procedure Clear (The_Token : in out Instance)
   is begin
      The_Token.State := First_Char;
   end Clear;

   overriding procedure Analyze
     (The_Token : in out Instance;
      Next_Char : in     Character;
      Verdict   :    out Analysis_Verdict)
   is begin
      case The_Token.State is
      when First_Char =>
         if Ada.Strings.Maps.Is_In (Element => Next_Char, Set => The_Token.Set) then
            Verdict := Matches;
            The_Token.State := Done;
         else
            Verdict         := Failed;
            The_Token.State := Done;
         end if;
      when Done =>
         --  We shouldn't get called from here.
         Verdict := Failed;
      end case;
   end Analyze;

   ----------------------------------------------------------------------------
   --  This procedure will be called to create a character set token
   ----------------------------------------------------------------------------
   function Get (Set        : in Ada.Strings.Maps.Character_Set)
                return Instance
   is begin
      return (Report => True,
              State  => First_Char,
              Set    => Set);
   end Get;

end OpenToken.Recognizer.Single_Character_Set;
