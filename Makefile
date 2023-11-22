#-*- mode: makefile-gmake -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

PROJECT = msmp
PROJECT_DESCRIPTION = MySQL/MariaDB Message Protocol
PROJECT_VERSION = ${shell git describe --tags}

COVER = 1

COVER_REPORT_DIR = _site/cover
CT_LOGS_DIR = _site/ct
EDOC_OPTS = {preprocess, true}, {dir, "_site/edoc"}

DEPS += narcs
DEPS += phrase
DEPS += scran

SHELL_DEPS += sync

PLT_APPS += compiler
PLT_APPS += crypto
PLT_APPS += narcs
PLT_APPS += phrase
PLT_APPS += scran
PLT_APPS += stdlib

SHELL_OPTS += +pc unicode
SHELL_OPTS += -config dev.config
SHELL_OPTS += -enable-feature maybe_expr
SHELL_OPTS += -s sync


dep_narcs = $(if $(DEP_LN),ln ../../narcs,git https://github.com/shortishly/narcs.git)
dep_phrase = $(if $(DEP_LN),ln ../../phrase,git https://github.com/shortishly/phrase.git)
dep_scran = $(if $(DEP_LN),ln ../../scran,git https://github.com/shortishly/scran.git)

define HEX_TARBALL_EXTRA_METADATA
#{
	licenses => [<<"Apache-2">>],
	links => #{
		<<"Function reference">> => <<"https://shortishly.github.io/msmp/edoc/">>,
		<<"Test Coverage">> => <<"https://shortishly.github.io/msmp/cover/">>,
		<<"GitHub">> => <<"https://github.com/shortishly/msmp">>
	}
}
endef

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

app:: rebar.config
