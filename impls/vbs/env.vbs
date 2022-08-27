
class enviroment
	public data
	private sub Class_Initialize()
		set data = CreateObject("Scripting.Dictionary")
		
	end sub

   public sub setOuter(outer)
      data.add "outer", outer
   end sub

   public sub set_(symbolKey,malValue)
      data.add symbolKey, malValue
   end sub

   public function find(symbolKey)
      if data.Exists(symbolKey) then
         set find = data
      else
         if data.item("outer") = empty then
            err.raise vbObjectError, "find", "not found, undefined symbol: " & symbolKey
         else
            set find = data.item("outer").find(symbolKey)
         end if
      end if
   end function

   public function get_(symbolKey)
      set get_ = find(symbolKey).item(symbolKey)
   end function
end class