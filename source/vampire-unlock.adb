-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Unlock;
with Vampire.Configurations;
procedure Vampire.Unlock is
begin
	Tabula.Unlock (
		Lock_Name => Configurations.Lock_Name'Access,
		Debug_Log_File_Name => Configurations.Debug_Log_File_Name'Access);
end Vampire.Unlock;
