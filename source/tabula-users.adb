-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Users is
	
	function Valid_Id_String(Id : String) return Boolean is 
	begin
		if Id = "" then
			return False;
		end if;
		for I in Id'Range loop
			case Id(I) is
				when 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' =>
					null;
				when others =>
					return False;
			end case;
		end loop;
		return True;
	end Valid_Id_String;
	
	function Digest(Password : String) return Ase.MD5.Message_Digest is
		MD5 : Ase.MD5.Context;
	begin
		Ase.MD5.Update(MD5, Password);
		return Ase.MD5.Image(Ase.MD5.Digest(MD5));
	end Digest;
	
end Tabula.Users;
