-module(kvconf_pkix).

-export([validate_pkix_fullchain_pem_file/1,
         validate_pkix_privkey_pem_file/1,
         validate_pkix_cert_pem_file/1]).


%% TODO: 複数 CA ファイル設定できる vlaidate_pkix_cacert_path で dir を指定できる仕組みを作る


validate_pkix_fullchain_pem_file(FullchainPemFile) ->
    case file:read_file(FullchainPemFile) of
        {ok, Bin} ->
            %% PEM or DER
            case public_key:pem_decode(Bin) of
                [] ->
                    error;
                PemEntryList when is_list(PemEntryList) ->
                    %% PemEntry が全て {'Certificate', _, not_encrypted} であることを確認する
                    F = fun({'Certificate', Der, not_encrypted}) ->
                                try
                                    %% 証明書がデコードできるか確認する
                                    _Entity = public_key:pkix_decode_cert(Der, otp),
                                    true
                                catch
                                    _:_ ->
                                        false
                                end;
                           (_) ->
                                false
                        end,
                    case lists:all(F, PemEntryList) of
                        true ->
                            %% TODO: 複数証明書が入ってた場合はチェーンを確認する
                            %% TODO: チェーンを確認するかどうかを指定できるようにする
                            ok;
                        false ->
                            error
                    end
            end;
        {error, _Reason} ->
            error
    end.


validate_pkix_privkey_pem_file(KeyfilePath) ->
    case file:read_file(KeyfilePath) of
        {ok, Bin} ->
            %% Format
            case public_key:pem_decode(Bin) of
                [] ->
                    error;
                %% not_encrypted であることを確認する
                [{_PkiAsn1Type, _Der, not_encrypted}] ->
                    %% TODO: 対応している PKI Asn1Type を指定できるようにする
                    ok;
                _ ->
                    error
            end;
        {error, _Reason} ->
            error
    end.


validate_pkix_cert_pem_file(CertPemFile) ->
    case file:read_file(CertPemFile) of
        {ok, Bin} ->
            %% Format
            case public_key:pem_decode(Bin) of
                [] ->
                    error;
                PemEntryList when is_list(PemEntryList) ->
                    %% PemEntry が全て {'Certificate', _, not_encrypted} であることを確認する
                    F = fun({'Certificate', Der, not_encrypted}) ->
                                try
                                    %% 証明書がデコードできるか確認する
                                    _Entity = public_key:pkix_decode_cert(Der, otp),
                                    true
                                catch
                                    _:_ ->
                                        false
                                end;
                           (_) ->
                                false
                        end,
                    case lists:all(F, PemEntryList) of
                        true ->
                            ok;
                        false ->
                            error
                    end
            end;
        {error, _Reason} ->
            error
    end.
