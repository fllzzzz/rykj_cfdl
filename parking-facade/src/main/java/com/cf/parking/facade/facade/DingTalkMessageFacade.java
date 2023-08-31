package com.cf.parking.facade.facade;

import com.cf.parking.facade.dto.CardMessageDTO;
import com.cf.parking.facade.dto.LinkMessageDTO;
import com.cf.parking.facade.dto.TextMessageDTO;

import java.util.List;

/**
 * @Classname SendDingTalkMessageFacade
 * @Date 2022/10/20 13:06
 * @Created by csy
 */
public interface DingTalkMessageFacade {
    /**
     * 异步批量发送消息，错误打印日志，不影响主题线程，
     *
     * @param messageDTOList 文字消息列表
     */
    void asyncSendBatchText(List<TextMessageDTO> messageDTOList);

    /**
     * 同步批量发送消息，
     *
     * @param messageDTOList 文字消息列表
     */
    void sendBatchText(List<TextMessageDTO> messageDTOList);

    /**
     * 异步批量发送消息，错误打印日志，不影响主题线程，
     *
     * @param messageDTOList 链接消息列表
     * @param title          标题名称
     */
    void asyncSendBatchLink(List<LinkMessageDTO> messageDTOList, String title);

    /**
     * 异步批量发送消息，错误打印日志，不影响主题线程，
     *
     * @param messageDTOList 链接消息列表
     * @param title          标题名称
     */
    void sendBatchLink(List<LinkMessageDTO> messageDTOList, String title);

    /**
     * 异步发送消息，错误打印日志，不影响主题线程，
     *
     * @param messageDTO 链接消息列表
     * @param title          标题名称
     */
    void asyncSendLink(LinkMessageDTO messageDTO, String title);

    /**
     * 异步发送消息，错误打印日志，不影响主题线程，
     *
     * @param messageDTO 链接消息列表
     * @param title          标题名称
     */
    void asyncSendCard(CardMessageDTO messageDTO, String title);

    /**
     * 异步批量发送消息，错误打印日志，不影响主题线程，
     *
     * @param messageDTOList 链接消息列表
     * @param title          标题名称
     */
    void asyncSendBatchCard(List<CardMessageDTO> messageDTOList, String title);
}
