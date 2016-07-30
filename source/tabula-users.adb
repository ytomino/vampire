-- The Village of Vampire by YT, このソースコードはNYSLです
with Crypto.MD5;
package body Tabula.Users is
	
	pragma Compile_Time_Error (
		Password_Digest'Length /= Crypto.MD5.Fingerprint'Length,
		"length of MD5 /= 16");
	
	pragma Compile_Time_Error (
		Password_Digest_Image'Length /= Crypto.MD5.Message_Digest'Length,
		"image length of MD5 /= 32");
	
	function Valid_Id_String (Id : String) return Boolean is
	begin
		if Id'Length = 0 then
			return False;
		end if;
		for I in Id'Range loop
			case Id (I) is
				when 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' =>
					null;
				when others =>
					return False;
			end case;
		end loop;
		return True;
	end Valid_Id_String;
	
	function Digest (Password : String) return Password_Digest is
	begin
		return Result : Password_Digest do
			declare
				MD5 : Crypto.MD5.Context := Crypto.MD5.Initial;
			begin
				Crypto.MD5.Update (MD5, Password);
				Crypto.MD5.Final (MD5, Crypto.MD5.Fingerprint (Result));
			end;
		end return;
	end Digest;
	
	function Image (Digest : Password_Digest) return Password_Digest_Image is
	begin
		return Crypto.MD5.Image (Crypto.MD5.Fingerprint (Digest));
	end Image;
	
	function Value (Image : Password_Digest_Image) return Password_Digest is
	begin
		return Password_Digest (Crypto.MD5.Value (Image));
	end Value;
	
end Tabula.Users;
