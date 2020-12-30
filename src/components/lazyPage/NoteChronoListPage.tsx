import React from 'react';
import {AddPageTitle} from '../Misc';
import {NotesChronoList} from '../List';

export default function NoteChronoListPage() {
  return <AddPageTitle pageTitle={"Notes"} page={<NotesChronoList />} />;
}
