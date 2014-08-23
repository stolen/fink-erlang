%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------


-record(state, {identity,
                level,
                retry_interval,
                retry_times,
                protocol,
                public_key,
                secret_key,
                project,
                port = 31338,
                socket = undefined,
                url = undefined,
                hostname = undefined,

                error = error,
                warning_error = error,
                info_msg = info,
                warning_msg = warn,
                error_report = error,
                info_report = info,
                warning_report = warn}).
