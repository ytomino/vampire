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
	
	function Digest(Password : String) return Crypto.MD5.Message_Digest is
		MD5 : Crypto.MD5.Context := Crypto.MD5.Initial;
		Digest : Crypto.MD5.Fingerprint;
	begin
		Crypto.MD5.Update(MD5, Password);
		Crypto.MD5.Final(MD5, Digest);
		return Crypto.MD5.Image(Digest);
	end Digest;
	
end Tabula.Users;
