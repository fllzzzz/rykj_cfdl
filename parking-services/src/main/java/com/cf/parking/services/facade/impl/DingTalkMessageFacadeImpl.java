package com.cf.parking.services.facade.impl;

import com.cf.parking.facade.dto.CardMessageDTO;
import com.cf.parking.facade.dto.LinkMessageDTO;
import com.cf.parking.facade.dto.TextMessageDTO;
import com.cf.parking.facade.facade.DingTalkMessageFacade;
import com.cf.parking.services.properties.DingTalkProperties;
import com.cf.support.bean.DingTalkBean;
import com.dingtalk.api.request.OapiMessageCorpconversationAsyncsendV2Request;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @Classname DingTalkMessageFacadeImpl
 * @Date 2022/10/20 13:12
 * @Created by csy
 */
@Service
@Slf4j
public class DingTalkMessageFacadeImpl implements DingTalkMessageFacade {
    @Autowired
    private DingTalkBean dingTalkBean;
    @Autowired
    private DingTalkProperties dingTalkProperties;

    @Override
    @Async
    public void asyncSendBatchText(List<TextMessageDTO> messageDTOList) {
        messageDTOList.forEach(messageDTO -> {
            try {
                dingTalkBean.sendTextMessage(messageDTO.getMessage(), messageDTO.getOpenIdList());
            } catch (Exception e) {
                log.error("send text message error", e);
            }
        });
    }

    @Override
    public void sendBatchText(List<TextMessageDTO> messageDTOList) {
        messageDTOList.forEach(messageDTO -> {
            dingTalkBean.sendTextMessage(messageDTO.getMessage(), messageDTO.getOpenIdList());
        });
    }

    @Override
    @Async
    public void asyncSendBatchLink(List<LinkMessageDTO> messageDTOList, String title) {
        messageDTOList.forEach(messageDTO -> {
            try {
                OapiMessageCorpconversationAsyncsendV2Request.Link link = new OapiMessageCorpconversationAsyncsendV2Request.Link();
                link.setTitle(title);
                link.setMessageUrl(messageDTO.getUrl());
                link.setText(messageDTO.getUrl());
                link.setPicUrl(dingTalkProperties.getImageId());
                dingTalkBean.sendLinkMessage(link, messageDTO.getOpenIdList());
            } catch (Exception e) {
                log.error("send text message error", e);
            }

        });
    }

    @Override
    public void sendBatchLink(List<LinkMessageDTO> messageDTOList, String title) {
        messageDTOList.forEach(messageDTO -> {
            OapiMessageCorpconversationAsyncsendV2Request.Link link = new OapiMessageCorpconversationAsyncsendV2Request.Link();
            link.setTitle(title);
            link.setMessageUrl(messageDTO.getUrl());
            link.setText(messageDTO.getUrl());
            link.setPicUrl(dingTalkProperties.getImageId());
            dingTalkBean.sendLinkMessage(link, messageDTO.getOpenIdList());
        });
    }

    @Override
    @Async
    public void asyncSendLink(LinkMessageDTO messageDTO, String title) {
        try {
            OapiMessageCorpconversationAsyncsendV2Request.Link link = new OapiMessageCorpconversationAsyncsendV2Request.Link();
            link.setTitle(title);
            link.setMessageUrl(messageDTO.getUrl());
            link.setText(messageDTO.getUrl());
            link.setPicUrl(dingTalkProperties.getImageId());
            dingTalkBean.sendLinkMessage(link, messageDTO.getOpenIdList());
        } catch (Exception e) {
            log.error("send text message error", e);
        }
    }

    @Override
    public void asyncSendCard(CardMessageDTO messageDTO, String title) {
        try {
            dingTalkBean.sendCardMessage(messageDTO.getMessage(), title, messageDTO.getUrl(), messageDTO.getOpenIdList());
        } catch (Exception e) {
            log.error("send card message error", e);
        }
    }

    @Override
    @Async
    public void asyncSendBatchCard(List<CardMessageDTO> messageDTOList, String title) {
        messageDTOList.forEach(messageDTO -> {
            dingTalkBean.sendCardMessage(messageDTO.getMessage(), title, messageDTO.getUrl(), messageDTO.getOpenIdList());
        });
    }
}
