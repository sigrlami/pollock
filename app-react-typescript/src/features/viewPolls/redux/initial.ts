import { IReduxState } from '../namespace';

const initial: IReduxState = {
  polls: [],
  selectedPoll: null,
  communication: {
    pollsLoading: {
      isRequesting: false,
      error: '',
    },
    answersSaving: {
      isRequesting: false,
      error: '',
    },
    pollLoading: {
      isRequesting: false,
      error: '',
    },
  },
};

export default initial;
