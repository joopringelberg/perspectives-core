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

module Persistence.Attachment where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign (F, Foreign, readNullOrUndefined)
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- CLASS ATTACHMENT
-----------------------------------------------------------
-- On reading a rolinstance document from Couchdb, we must take attachments into 
-- account. Attachments are added to the outer 'envelope' of the json document
-- by Couchdb. We retrieve these (if any) and put them in the inner "contents"
-- section of the representation created by generic decode.
class Attachment a where
  setAttachment :: a -> Maybe Attachments -> a
  getAttachments :: a -> Maybe Attachments

-----------------------------------------------------------
-- NEWTYPE ATTACHMENTS
-----------------------------------------------------------
newtype Attachments = Attachments Foreign
derive instance Newtype Attachments _
instance Show Attachments where show _ = ""
instance Eq Attachments where eq _ _ = true
derive newtype instance WriteForeign Attachments
derive newtype instance ReadForeign Attachments

foreign import getAttachmentsImpl :: Foreign -> Foreign

getRawAttachments :: Foreign -> F (Maybe Attachments)
getRawAttachments f = do
  (x :: Maybe Foreign) <- readNullOrUndefined (getAttachmentsImpl f)
  case x of 
    Nothing -> pure Nothing
    Just r -> pure $ Just $ Attachments r