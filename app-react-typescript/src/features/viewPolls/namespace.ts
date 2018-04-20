import { ICommunication, IPoll, IPollResult } from 'shared/types/app';

interface IReduxState {
  selectedPoll: {
    poll: IPoll;
    results: IPollResult[];
  } | null;
  polls: IPoll[];
  communication: {
    pollsLoading: ICommunication;
    pollLoading: ICommunication;
    answersSaving: ICommunication;
  };
}

export { IReduxState };
