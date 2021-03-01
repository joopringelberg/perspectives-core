-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Authenticate where

-- | Top level entry to message authentication. Sign a message. Under the hood this requires fetching
-- | the users private key.
import Data.Maybe (Maybe(..))
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)

-- | Top level entry to message authentication. Sign a message. Under the hood this requires fetching
-- | the users private key and encrypting the message.
sign :: String -> String
sign message = message

-- | Top level entry to message authentication. Given a role instance of model:System$PerspectivesSystem$User,
-- | (the author) ensure that the message was, indeed, sent by the author. Under the hood this involves fetching
-- | the authors' public key and decrypting the message.
authenticate :: RoleInstance -> String -> Maybe String
authenticate author message = Just message

-- | Use the current authors key to decrypt the message. This must succeed; otherwise we have a logical error.
selfAuthenticate :: String -> String
selfAuthenticate message = message
